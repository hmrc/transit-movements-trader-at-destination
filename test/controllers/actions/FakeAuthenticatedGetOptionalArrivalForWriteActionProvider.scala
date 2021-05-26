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

import models.Arrival
import models.ChannelType.web
import models.request.AuthenticatedOptionalArrivalRequest
import models.request.AuthenticatedRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.BodyParser
import play.api.mvc.Request
import play.api.mvc.Result

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global

class FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(arrival: Option[Arrival]) extends AuthenticatedGetOptionalArrivalForWriteActionProvider {
  override def apply(): ActionBuilder[AuthenticatedOptionalArrivalRequest, AnyContent] =
    new ActionBuilder[AuthenticatedOptionalArrivalRequest, AnyContent] {
      override def parser: BodyParser[AnyContent] = stubBodyParser()

      override def invokeBlock[A](request: Request[A], block: AuthenticatedOptionalArrivalRequest[A] => Future[Result]): Future[Result] = {
        val eoriNumber                                  = arrival.fold("eori")(_.eoriNumber)
        val authReq                                     = AuthenticatedRequest(request, web, eoriNumber)
        val req: AuthenticatedOptionalArrivalRequest[A] = AuthenticatedOptionalArrivalRequest(authReq, arrival, web, eoriNumber)
        block(req)
      }

      override protected def executionContext: ExecutionContext = implicitly[ExecutionContext]
    }
}

object FakeAuthenticatedGetOptionalArrivalForWriteActionProvider {

  def apply(arrival: Arrival): FakeAuthenticatedGetOptionalArrivalForWriteActionProvider =
    new FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(Some(arrival))

  def apply(): FakeAuthenticatedGetOptionalArrivalForWriteActionProvider =
    new FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(None)
}
