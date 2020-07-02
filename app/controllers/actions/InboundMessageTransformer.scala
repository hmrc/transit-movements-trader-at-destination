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

package controllers.actions
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import models.ArrivalRejectedResponse
import models.GoodsReleasedResponse
import models.MessageInbound
import models.MessageResponse
import models.MessageType
import models.UnloadingPermissionResponse
import models.UnloadingRemarksRejectedResponse
import models.request.ArrivalRequest
import play.api.Logger
import play.api.mvc.ActionTransformer
import play.api.mvc.WrappedRequest

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class InboundMessageTransformer @Inject()(implicit ec: ExecutionContext) extends InboundMessageTransformerInterface {

  def executionContext: ExecutionContext = ec

  override def transform[A](request: ArrivalRequest[A]): Future[InboundRequest[A]] = {

    val inboundMessage: Option[MessageResponse] = request.headers.get("X-Message-Type") match {
      case Some(MessageType.GoodsReleased.code)             => Some(GoodsReleasedResponse)
      case Some(MessageType.ArrivalRejection.code)          => Some(ArrivalRejectedResponse)
      case Some(MessageType.UnloadingPermission.code)       => Some(UnloadingPermissionResponse)
      case Some(MessageType.UnloadingRemarksRejection.code) => Some(UnloadingRemarksRejectedResponse)
      case invalidResponse =>
        Logger.error(s"Unsupported X-Message-Type: $invalidResponse")
        None
    }

    inboundMessage match {
      case Some(response) =>
        val nextState = request.arrival.status.transition(response.messageReceived)

        Future.successful(
          new InboundRequest(Some(MessageInbound(response, nextState)), request)
        )
      case None =>
        Future.successful(
          new InboundRequest(None, request)
        )
    }

  }

}

@ImplementedBy(classOf[InboundMessageTransformer])
trait InboundMessageTransformerInterface extends ActionTransformer[ArrivalRequest, InboundRequest]

class InboundRequest[A](val inboundMessage: Option[MessageInbound], val request: ArrivalRequest[A]) extends WrappedRequest[A](request)
