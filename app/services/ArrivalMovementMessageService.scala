/*
 * Copyright 2022 HM Revenue & Customs
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
import models.Arrival
import models.ArrivalId
import models.Box
import models.ChannelType
import models.EnrolmentId
import models.MessageId
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import models.MessageStatus.SubmissionPending
import models.ParseError.EmptyNodeSeq
import repositories.ArrivalIdRepository
import utils.XMLTransformer

import java.time.Clock
import java.time.LocalDateTime
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class ArrivalMovementMessageService @Inject()(arrivalIdRepository: ArrivalIdRepository, clock: Clock)(implicit ec: ExecutionContext) {
  import XMLTransformer._
  import XmlMessageParser._

  def makeArrivalMovement(enrolmentId: EnrolmentId, nodeSeq: NodeSeq, channelType: ChannelType, boxOpt: Option[Box]): Future[ParseHandler[Arrival]] =
    arrivalIdRepository.nextId().map {
      arrivalId =>
        (for {
          _        <- correctRootNodeR(MessageType.ArrivalNotification)
          dateTime <- dateTimeOfPrepR
          message  <- makeOutboundMessage(arrivalId, MessageId(1), messageCorrelationId = 1, messageType = MessageType.ArrivalNotification)
          mrn      <- mrnR
        } yield
          Arrival(
            arrivalId,
            channelType,
            mrn,
            enrolmentId.customerId,
            dateTime,
            dateTime,
            LocalDateTime.now(clock),
            NonEmptyList.one(message),
            2,
            boxOpt
          )).apply(nodeSeq)
    }

  def messageAndMrn(
    arrivalId: ArrivalId,
    messageId: MessageId,
    messageCorrectionId: Int
  ): ReaderT[ParseHandler, NodeSeq, (MovementMessageWithStatus, MovementReferenceNumber)] =
    for {
      _       <- correctRootNodeR(MessageType.ArrivalNotification)
      message <- makeOutboundMessage(arrivalId, messageId, messageCorrectionId, MessageType.ArrivalNotification)
      mrn     <- mrnR
    } yield (message, mrn)

  def makeInboundMessage(
    messageId: MessageId,
    messageCorrelationId: Int,
    messageType: MessageType
  ): ReaderT[ParseHandler, NodeSeq, MovementMessageWithoutStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[ParseHandler, NodeSeq, NodeSeq](nodeSeqToEither)
    } yield MovementMessageWithoutStatus(messageId, dateTime, messageType, xmlMessage, messageCorrelationId)

  def makeOutboundMessage(
    arrivalId: ArrivalId,
    messageId: MessageId,
    messageCorrelationId: Int,
    messageType: MessageType
  ): ReaderT[ParseHandler, NodeSeq, MovementMessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- updateMesSenMES3(arrivalId, messageCorrelationId)
    } yield MovementMessageWithStatus(messageId, dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)

  private[this] def nodeSeqToEither(xml: NodeSeq): ParseHandler[NodeSeq] =
    Option(xml).fold[ParseHandler[NodeSeq]](
      ifEmpty = Left(EmptyNodeSeq("Request body is empty"))
    )(
      xml => Right(xml)
    )
}
