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

import com.google.inject.Inject
import models.ArrivalStatus.GoodsReleased
import models.request.ArrivalRequest
import models.GoodsReleasedResponse
import models.MessageInbound
import play.api.mvc.Result
import play.api.mvc.Results.BadRequest

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class FakeInboundMessageTransformer @Inject()(implicit ec: ExecutionContext) extends InboundMessageTransformerInterface {

  override def executionContext: ExecutionContext = ec

  override protected def refine[A](request: ArrivalRequest[A]): Future[Either[Result, InboundRequest[A]]] =
    Future.successful(
      Right(InboundRequest(MessageInbound(GoodsReleasedResponse, GoodsReleased), request))
    )
}

class FakeInboundMessageBadRequestTransformer @Inject()(implicit ec: ExecutionContext) extends InboundMessageTransformerInterface {

  override def executionContext: ExecutionContext = ec

  override protected def refine[A](request: ArrivalRequest[A]): Future[Either[Result, InboundRequest[A]]] =
    Future.successful(
      Left(BadRequest)
    )
}
