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
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import repositories.ArrivalIdRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class ArrivalMovementMessageService @Inject()(arrivalIdRepository: ArrivalIdRepository)(implicit ec: ExecutionContext) {
  import CtcXmlMessageParser._

  def makeArrivalMovement(eori: String): ReaderT[Option, NodeSeq, Future[Arrival]] =
    for {
      _        <- correctRootNodeR(MessageType.ArrivalNotification)
      dateTime <- dateTimeOfPrepR
      message  <- makeMovementMessageWithStatus(1, MessageType.ArrivalNotification)
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

  def makeMessage(messageCorrelationId: Int, messageType: MessageType): ReaderT[Option, NodeSeq, MovementMessageWithoutStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield MovementMessageWithoutStatus(dateTime, messageType, xmlMessage, messageCorrelationId)

  def makeMovementMessageWithStatus(messageCorrelationId: Int, messageType: MessageType): ReaderT[Option, NodeSeq, MovementMessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield MovementMessageWithStatus(dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)
}
