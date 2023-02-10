/*
 * Copyright 2023 HM Revenue & Customs
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

import models.ArrivalId
import models.MessageType
import models.request.ArrivalsMessagesRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.DefaultActionBuilder

import javax.inject.Inject

trait AuthenticatedGetMessagesForReadActionProvider {
  def apply(arrivalId: ArrivalId, messageTypes: List[MessageType]): ActionBuilder[ArrivalsMessagesRequest, AnyContent]
}

class AuthenticatedGetMessagesForReadActionProviderImpl @Inject()(
  authenticate: AuthenticateActionProvider,
  getArrivalMessages: AuthenticatedGetMessagesActionProvider,
  buildDefault: DefaultActionBuilder
) extends AuthenticatedGetMessagesForReadActionProvider {

  def apply(arrivalId: ArrivalId, messageTypes: List[MessageType]): ActionBuilder[ArrivalsMessagesRequest, AnyContent] =
    buildDefault andThen authenticate() andThen getArrivalMessages(arrivalId, messageTypes)
}
