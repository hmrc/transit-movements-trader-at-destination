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
import models.Message
import models.OutboundMessage
import models.OutboundMessageResponse
import models.request.ArrivalRequest
import models.request.actions.MessageTransformRequest
import play.api.mvc.Results.InternalServerError
import play.api.mvc.ActionRefiner
import play.api.mvc.Result
import play.api.mvc.WrappedRequest

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class OutboundMessageTransformer @Inject()(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[MessageTransformRequest, OutboundMessageRequest]
    with Logging {
  override protected def refine[A](request: MessageTransformRequest[A]): Future[Either[Result, OutboundMessageRequest[A]]] =
    request.message.messageType match {
      case message: OutboundMessageResponse =>
        Future.successful(Right(OutboundMessageRequest(message = OutboundMessage(message, request.message.nextState), arrivalRequest = request.arrivalRequest)))
      case _ =>
        logger.error("[refine] Found an inbound message when expecting an outbound message ( INBOUND_MESSAGE_FOUND_FOR_OUTBOUND_MESSAGE )")
        Future.successful(Left(InternalServerError(s"Found Inbound Message when expecting an Outbound Message")))
    }
}

case class OutboundMessageRequest[A](message: OutboundMessage, arrivalRequest: ArrivalRequest[A]) extends WrappedRequest[A](arrivalRequest)
