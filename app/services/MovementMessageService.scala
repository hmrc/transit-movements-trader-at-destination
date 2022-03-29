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

import cats.data.EitherT
import models.FailedToMakeMovementMessage
import models.MessageId
import models.MessageType
import models.MovementMessageWithoutStatus
import models.SubmissionState

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementMessageService @Inject() (arrivalMovementMessageService: ArrivalMovementMessageService)(implicit ec: ExecutionContext) {

  def makeMovementMessage(messageCorrelationId: Int, messageType: MessageType, xml: NodeSeq): EitherT[Future, SubmissionState, MovementMessageWithoutStatus] =
    EitherT.fromEither(
      arrivalMovementMessageService
        .makeInboundMessage(MessageId(0), messageCorrelationId, messageType)(xml)
        .left
        .map {
          parseError =>
            FailedToMakeMovementMessage(s"Failed to make movement message: ${parseError.message}")
        }
    )
}
