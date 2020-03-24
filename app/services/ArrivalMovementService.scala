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
import java.time.LocalTime

import cats.data._
import cats.implicits._
import com.google.inject.Inject
import models.Arrival
import models.MessageType
import models.MovementMessage
import models.messages.MovementReferenceNumber
import repositories.ArrivalIdRepository
import utils.Format
import models.State.PendingSubmission
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import scala.xml.NodeSeq

class ArrivalMovementService @Inject()(arrivalIdRepository: ArrivalIdRepository)(implicit ec: ExecutionContext) {
  import ArrivalMovementService._

  def makeArrivalMovement(eori: String): ReaderT[Option, NodeSeq, Future[Arrival]] =
    for {
      _          <- correctRootNodeR(MessageType.ArrivalNotification)
      date       <- dateOfPrepR
      time       <- timeOfPrepR
      mrn        <- mrnR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield {
      arrivalIdRepository
        .nextId()
        .map(Arrival(_, mrn.value, eori, PendingSubmission, Seq(MovementMessage(date, time, MessageType.ArrivalNotification, xmlMessage))))
    }

  def makeGoodsReleasedMessage(): ReaderT[Option, NodeSeq, MovementMessage] =
    for {
      _          <- correctRootNodeR(MessageType.GoodsReleased)
      date       <- dateOfPrepR
      time       <- timeOfPrepR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield MovementMessage(date, time, MessageType.GoodsReleased, xmlMessage)
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

  val mrnR: ReaderT[Option, NodeSeq, MovementReferenceNumber] =
    ReaderT[Option, NodeSeq, MovementReferenceNumber](xml =>
      (xml \ "HEAHEA" \ "DocNumHEA5").text match {
        case mrnString if !mrnString.isEmpty => Some(MovementReferenceNumber(mrnString))
        case _                               => None
    })
}
