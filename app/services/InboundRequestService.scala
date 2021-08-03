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

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class InboundRequestService @Inject()(
  lockService: LockService,
  getArrivalService: GetArrivalService,
  xmlValidationService: XmlValidationService,
  arrivalMovementMessageService: ArrivalMovementMessageService,
)(implicit ec: ExecutionContext) {

  def makeInboundRequest(arrivalId: ArrivalId, xml: NodeSeq, messageSender: MessageSender): Future[Either[SubmissionState, InboundMessageRequest]] =
    (
      for {
        lock                   <- EitherT(lockService.lock(arrivalId))
        inboundMessageResponse <- makeInboundMessageResponse(xml)
        inboundMessage         <- makeMovementMessage(messageSender.messageCorrelationId, inboundMessageResponse.messageType, xml)
        arrival                <- EitherT(getArrivalService.getArrivalAndAudit(arrivalId, inboundMessageResponse, inboundMessage))
        updatedInboundMessage  = inboundMessage.copy(messageCorrelationId = arrival.nextMessageCorrelationId)
        nextStatus <- EitherT.fromEither(StatusTransition.transition(arrival.status, inboundMessageResponse.messageReceived))
        unlock     <- EitherT(lockService.unlock(arrivalId))
      } yield InboundMessageRequest(arrival, nextStatus, inboundMessageResponse, updatedInboundMessage)
    ).value

  // TODO move to service
  private def makeMovementMessage(messageCorrelationId: Int,
                                  messageType: MessageType,
                                  xml: NodeSeq): EitherT[Future, SubmissionState, MovementMessageWithoutStatus] =
    EitherT.fromEither(
      arrivalMovementMessageService
        .makeInboundMessage(MessageId(0), messageCorrelationId, messageType)(xml) // TODO what to do with this message id
        .toOption
        .toRight[SubmissionState](FailedToValidateMessage("error")))

  // TODO move to service
  private def makeInboundMessageResponse(xml: NodeSeq): EitherT[Future, SubmissionState, InboundMessageResponse] =
    for {
      headNode                <- EitherT.fromOption(xml.headOption, CannotFindRootNodeError(s"[InboundRequest][inboundRequest] Could not find root node"))
      messageResponse         <- EitherT.fromEither(MessageResponse.getMessageResponseFromCode(headNode.label))
      validateInboundResponse <- EitherT.fromEither(MessageValidationService.validateInboundMessage(messageResponse))
      validateXml <- EitherT.fromEither(
        xmlValidationService
          .validate(xml.toString, validateInboundResponse.xsdFile)
          .toOption
          .toRight[SubmissionState](FailedToValidateMessage(s"[InboundRequest][makeInboundMessageResponse] XML failed to validate against XSD file")))
    } yield validateInboundResponse
}
