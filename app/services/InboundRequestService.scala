/*
 * Copyright 2021 HM Revenue & Customs
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
import cats.implicits.catsStdInstancesForFuture
import models.ArrivalId
import models.CannotFindRootNodeError
import models.FailedToValidateMessage
import models.InboundMessageRequest
import models.InboundMessageResponse
import models.MessageId
import models.MessageResponse
import models.MessageSender
import models.MessageType
import models.MovementMessageWithoutStatus
import models.StatusTransition
import models.SubmissionState
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class InboundRequestService @Inject()(
  lockService: LockService,
  getArrivalService: GetArrivalService,
  inboundMessageResponseService: InboundMessageResponseService,
  movementMessageService: MovementMessageService
)(implicit ec: ExecutionContext) {

  def makeInboundRequest(arrivalId: ArrivalId, xml: NodeSeq, messageSender: MessageSender)(
    implicit hc: HeaderCarrier): Future[Either[SubmissionState, InboundMessageRequest]] =
    (
      for {
        lock                   <- EitherT(lockService.lock(arrivalId))
        inboundMessageResponse <- inboundMessageResponseService.makeInboundMessageResponse(xml)
        inboundMessage         <- movementMessageService.makeMovementMessage(messageSender.messageCorrelationId, inboundMessageResponse.messageType, xml)
        arrival                <- getArrivalService.getArrivalAndAudit(arrivalId, inboundMessageResponse, inboundMessage)
        updatedInboundMessage = inboundMessage.copy(messageId = arrival.nextMessageId)
        nextStatus <- EitherT.fromEither(StatusTransition.transition(arrival.status, inboundMessageResponse.messageReceived))
        unlock     <- EitherT(lockService.unlock(arrivalId))
      } yield InboundMessageRequest(arrival, nextStatus, inboundMessageResponse, updatedInboundMessage)
    ).value
}