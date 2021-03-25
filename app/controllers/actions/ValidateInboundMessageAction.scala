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

package controllers.actions

import logging.Logging
import models.InboundMessage
import models.InboundMessageResponse
import models.request.ArrivalRequest
import play.api.mvc.Results.BadRequest
import play.api.mvc.ActionRefiner
import play.api.mvc.Result
import play.api.mvc.WrappedRequest

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ValidateInboundMessageAction @Inject()(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[MessageTransformRequest, InboundMessageRequest]
    with Logging {
  override def refine[A](request: MessageTransformRequest[A]): Future[Either[Result, InboundMessageRequest[A]]] =
    request.message.messageType match {
      case message: InboundMessageResponse =>
        Future.successful(Right(InboundMessageRequest(message = InboundMessage(message, request.message.nextState), arrivalRequest = request.arrivalRequest)))
      case message =>
        logger.warn(
          s"Found an outbound message (${message.messageType}) when expecting an inbound message for arrivalId: ${request.arrivalRequest.arrival.arrivalId.index} ( OUTBOUND_MESSAGE_FOUND_FOR_INBOUND_MESSAGE )")
        Future.successful(Left(BadRequest(s"Unsupported X-Message-Type ${request.headers.get("X-Message-Type")}")))
    }
}

case class InboundMessageRequest[A](message: InboundMessage, arrivalRequest: ArrivalRequest[A]) extends WrappedRequest[A](arrivalRequest)
