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

package models.request.actions

import com.google.inject.ImplementedBy
import com.google.inject.Inject
import logging.Logging
import models.ArrivalRejectedResponse
import models.ChannelType
import models.GoodsReleasedResponse
import models.MessageInbound
import models.MessageResponse
import models.MessageType
import models.UnloadingPermissionResponse
import models.UnloadingRemarksRejectedResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import models.request.ArrivalRequest
import play.api.mvc.Results.BadRequest
import play.api.mvc._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class InboundMessageTransformer @Inject()(implicit ec: ExecutionContext) extends InboundMessageTransformerInterface with Logging {

  def executionContext: ExecutionContext = ec

  override protected def refine[A](request: ArrivalRequest[A]): Future[Either[Result, InboundRequest[A]]] = {

    logger.debug(s"CTC Headers ${request.headers}")

    messageResponse(request.headers.get("X-Message-Type"), request.channel) match {
      case Some(response) =>
        logger.debug(s"X-Message-Type is ${request.headers.get("X-Message-Type")}")
        logger.debug(s"Message type code ${response.messageType.code}")

        request.arrival.status.transition(response.messageReceived) match {
          case Right(nextState) =>
            logger.debug(s"Next state $nextState")

            Future.successful(
              Right(InboundRequest(MessageInbound(response, nextState), request))
            )
          case Left(error) =>
            logger.warn(s"Unable to transition movement state ${error.reason}")
            Future.successful(Left(badRequestError(error.reason)))
        }
      case None =>
        logger.warn(s"Unsupported X-Message-Type ${request.headers.get("X-Message-Type")}")

        Future.successful(
          Left(badRequestError(s"Unsupported X-Message-Type: ${request.headers.get("X-Message-Type")}"))
        )
    }
  }

  //TODO: Consider moving this into MessageResponse
  private[models] def messageResponse(code: Option[String], channel: ChannelType): Option[MessageResponse] = code match {
    case Some(MessageType.GoodsReleased.code)             => Some(GoodsReleasedResponse)
    case Some(MessageType.ArrivalRejection.code)          => Some(ArrivalRejectedResponse)
    case Some(MessageType.UnloadingPermission.code)       => Some(UnloadingPermissionResponse)
    case Some(MessageType.UnloadingRemarksRejection.code) => Some(UnloadingRemarksRejectedResponse)
    case Some(MessageType.XMLSubmissionNegativeAcknowledgement.code) =>
      logger.error(s"Received the message ${MessageType.XMLSubmissionNegativeAcknowledgement.code} for the $channel channel")
      Some(XMLSubmissionNegativeAcknowledgementResponse)
    case _ => None
  }

  private def badRequestError(message: String): Result = {
    logger.error(message)
    BadRequest(message)
  }

}

@ImplementedBy(classOf[InboundMessageTransformer])
trait InboundMessageTransformerInterface extends ActionRefiner[ArrivalRequest, InboundRequest]

case class InboundRequest[A](val inboundMessage: MessageInbound, val request: ArrivalRequest[A]) extends WrappedRequest[A](request)
