/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import cats.data._
import cats.implicits._
import com.google.inject.Inject
import models.ArrivalState.Initialized
import models.MessageState.SubmissionPending
import models.Arrival
import models.MessageType
import models.MovementMessage
import models.MovementMessageWithState
import models.MovementMessageWithoutState
import models.MovementReferenceNumber
import repositories.ArrivalIdRepository
import repositories.ArrivalMovementRepository
import utils.Format

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import scala.xml.NodeSeq

class ArrivalMovementService @Inject()(arrivalIdRepository: ArrivalIdRepository, arrivalMovementRepository: ArrivalMovementRepository)(
  implicit ec: ExecutionContext) {
  import ArrivalMovementService._

  def getArrivalMovement(eoriNumber: String, xml: NodeSeq): Future[Option[Arrival]] =
    mrnR(xml).map(arrivalMovementRepository.get(eoriNumber, _)).getOrElse(Future.successful(None))

  def makeArrivalMovement(eori: String): ReaderT[Option, NodeSeq, Future[Arrival]] =
    for {
      _        <- correctRootNodeR(MessageType.ArrivalNotification)
      dateTime <- dateTimeOfPrepR
      message  <- makeArrivalNotificationMessage(1)
      mrn      <- mrnR
    } yield {
      arrivalIdRepository
        .nextId()
        .map(
          Arrival(
            _,
            mrn,
            eori,
            Initialized,
            dateTime,
            dateTime,
            Seq(message),
            2
          ))
    }

  def makeArrivalNotificationMessage(messageCorrelationId: Int): ReaderT[Option, NodeSeq, MovementMessageWithState] =
    for {
      _          <- correctRootNodeR(MessageType.ArrivalNotification)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield MovementMessageWithState(dateTime, MessageType.ArrivalNotification, xmlMessage, SubmissionPending, messageCorrelationId)

  def makeGoodsReleasedMessage(messageCorrelationId: Int): ReaderT[Option, NodeSeq, MovementMessageWithoutState] =
    for {
      _          <- correctRootNodeR(MessageType.GoodsReleased)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield MovementMessage(dateTime, MessageType.GoodsReleased, xmlMessage, messageCorrelationId)
}

object ArrivalMovementService {

  def correctRootNodeR(messageType: MessageType): ReaderT[Option, NodeSeq, Unit] =
    ReaderT[Option, NodeSeq, Unit] {
      nodeSeq =>
        if (nodeSeq.head.label == messageType.rootNode) Some(()) else None
    }

  val dateOfPrepR: ReaderT[Option, NodeSeq, LocalDate] =
    ReaderT[Option, NodeSeq, LocalDate](xml => {
      (xml \ "DatOfPreMES9").text match {
        case x if x.isEmpty => None
        case x => {
          Try {
            LocalDate.parse(x, Format.dateFormatter)
          }.toOption // TODO: We are not propagating this failure back, do we need to do this?
        }
      }
    })

  val timeOfPrepR: ReaderT[Option, NodeSeq, LocalTime] =
    ReaderT[Option, NodeSeq, LocalTime](xml => {
      (xml \ "TimOfPreMES10").text match {
        case x if x.isEmpty => None
        case x => {
          Try {
            LocalTime.parse(x, Format.timeFormatter)
          }.toOption // TODO: We are not propagating this failure back, do we need to do this?
        }
      }
    })

  val dateTimeOfPrepR: ReaderT[Option, NodeSeq, LocalDateTime] =
    for {
      date <- dateOfPrepR
      time <- timeOfPrepR
    } yield LocalDateTime.of(date, time)

  val mrnR: ReaderT[Option, NodeSeq, MovementReferenceNumber] =
    ReaderT[Option, NodeSeq, MovementReferenceNumber](xml =>
      (xml \ "HEAHEA" \ "DocNumHEA5").text match {
        case mrnString if !mrnString.isEmpty => Some(MovementReferenceNumber(mrnString))
        case _                               => None
    })
}
