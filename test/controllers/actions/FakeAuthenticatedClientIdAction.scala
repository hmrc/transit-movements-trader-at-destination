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

import javax.inject.Inject
import models.ChannelType.web
import models.request.AuthenticatedClientRequest
import play.api.mvc._
import play.api.test.Helpers.stubBodyParser

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FakeAuthenticatedClientIdActionProvider @Inject()() extends AuthenticatedClientIdActionProvider {

  override def apply(): ActionBuilder[AuthenticatedClientRequest, AnyContent] =
    new ActionBuilder[AuthenticatedClientRequest, AnyContent] {
      override def parser: BodyParser[AnyContent] = stubBodyParser()

      override def invokeBlock[A](request: Request[A], block: AuthenticatedClientRequest[A] => Future[Result]): Future[Result] =
        block(AuthenticatedClientRequest(request, web, "eori", Some("clientId")))

      override protected def executionContext: ExecutionContext = implicitly[ExecutionContext]
    }
}

object FakeAuthenticatedClientIdActionProvider {

  def apply(): FakeAuthenticatedClientIdActionProvider =
    new FakeAuthenticatedClientIdActionProvider()
}
