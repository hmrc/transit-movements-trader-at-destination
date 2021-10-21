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

import com.google.inject.ImplementedBy
import com.google.inject.Inject
import logging.Logging
import models._
import models.request.ArrivalWithoutMessagesRequest
import play.api.mvc.Results.BadRequest
import play.api.mvc._
import play.twirl.api.HtmlFormat

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessageTransformer @Inject()(implicit val executionContext: ExecutionContext) extends MessageTransformerInterface with Logging {

  override def refine[A](request: ArrivalWithoutMessagesRequest[A]): Future[Either[Result, MessageTransformRequest[A]]] =
    request.body match {
      case x: NodeSeq =>
        x.headOption.flatMap(node => messageResponse(request.channel)(node.label)) match {
          case Some(response) =>
            Future.successful(Right(MessageTransformRequest(Message(response), request)))
          case None =>
            logger.warn(s"Unsupported root node ${x.headOption.map(_.label)} and message type ${request.headers.get("X-Message-Type")}")
            Future.successful(
              Left(badRequestError(s"Unsupported root node: ${x.headOption.map(_.label)} X-Message-Type: ${request.headers.get("X-Message-Type")}")))
        }
      case invalidBody =>
        logger.warn(s"Invalid request body. Expected XML (NodeSeq) got: ${invalidBody.getClass}")
        Future.successful(Left(BadRequest(HtmlFormat.empty)))
    }

  //TODO: This mapping should have ALL message types. Consider moving this into MessageResponse.
  private[actions] def messageResponse(channel: ChannelType)(code: String): Option[MessageResponse] =
    code match {
      case MessageType.GoodsReleased.rootNode             => Some(GoodsReleasedResponse)
      case MessageType.ArrivalRejection.rootNode          => Some(ArrivalRejectedResponse)
      case MessageType.UnloadingPermission.rootNode       => Some(UnloadingPermissionResponse)
      case MessageType.UnloadingRemarks.rootNode          => Some(UnloadingRemarksResponse)
      case MessageType.UnloadingRemarksRejection.rootNode => Some(UnloadingRemarksRejectedResponse)
      case MessageType.XMLSubmissionNegativeAcknowledgement.rootNode =>
        logger.error(s"Received the message ${MessageType.XMLSubmissionNegativeAcknowledgement.code} for the $channel channel")
        Some(XMLSubmissionNegativeAcknowledgementResponse)
      case _ => None
    }

  private def badRequestError(message: String): Result = {
    logger.error(message)
    BadRequest(message)
  }
}

@ImplementedBy(classOf[MessageTransformer])
trait MessageTransformerInterface extends ActionRefiner[ArrivalWithoutMessagesRequest, MessageTransformRequest]

case class MessageTransformRequest[A](message: Message, arrivalRequest: ArrivalWithoutMessagesRequest[A]) extends WrappedRequest[A](arrivalRequest)
