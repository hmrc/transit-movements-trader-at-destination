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

import cats.data._
import cats.implicits._
import com.google.inject.Inject
import models.ArrivalStatus.Initialized
import models.MessageStatus.SubmissionPending
import models.Arrival
import models.ArrivalId
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import models.ParseError.EmptyNodeSeq
import repositories.ArrivalIdRepository
import utils.XMLTransformer

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class ArrivalMovementMessageService @Inject()(arrivalIdRepository: ArrivalIdRepository)(implicit ec: ExecutionContext) {
  import XMLTransformer._
  import XmlMessageParser._

  def makeArrivalMovement(eori: String, nodeSeq: NodeSeq): Future[ParseHandler[Arrival]] =
    arrivalIdRepository.nextId().map {
      arrivalId =>
        (for {
          _        <- correctRootNodeR(MessageType.ArrivalNotification)
          dateTime <- dateTimeOfPrepR
          message  <- makeMovementMessageWithStatus(arrivalId, messageCorrelationId = 1, messageType = MessageType.ArrivalNotification)
          mrn      <- mrnR
        } yield {

          Arrival(
            arrivalId,
            mrn,
            eori,
            Initialized,
            dateTime,
            dateTime,
            dateTime,
            NonEmptyList.one(message),
            2
          )
        }).apply(nodeSeq)
    }

  def messageAndMrn(arrivalId: ArrivalId, messageCorrectionId: Int): ReaderT[ParseHandler, NodeSeq, (MovementMessageWithStatus, MovementReferenceNumber)] =
    for {
      _       <- correctRootNodeR(MessageType.ArrivalNotification)
      message <- makeMovementMessageWithStatus(arrivalId, messageCorrectionId, MessageType.ArrivalNotification)
      mrn     <- mrnR
    } yield (message, mrn)

  def makeMessage(messageCorrelationId: Int, messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MovementMessageWithoutStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[ParseHandler, NodeSeq, NodeSeq](nodeSeqToEither)
    } yield MovementMessageWithoutStatus(dateTime, messageType, xmlMessage, messageCorrelationId)

  //TODO: Not sure if this is used anymore
  def makeMovementMessageWithStatus(messageCorrelationId: Int, messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MovementMessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[ParseHandler, NodeSeq, NodeSeq](nodeSeqToEither)
    } yield MovementMessageWithStatus(dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)

  def makeMovementMessageWithStatus(arrivalId: ArrivalId,
                                    messageCorrelationId: Int,
                                    messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MovementMessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- updateMesSenMES3(arrivalId, messageCorrelationId)
    } yield MovementMessageWithStatus(dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)

  private[this] def nodeSeqToEither(xml: NodeSeq): ParseHandler[NodeSeq] =
    if (xml != null) {
      Right(xml)
    } else {
      Left(EmptyNodeSeq("Request body is empty"))
    }
}
