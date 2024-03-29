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

import javax.inject.Inject
import models.ArrivalId
import models.request.ArrivalWithoutMessagesRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.DefaultActionBuilder

trait AuthenticatedGetArrivalWithoutMessagesForReadActionProvider {
  def apply(arrivalId: ArrivalId): ActionBuilder[ArrivalWithoutMessagesRequest, AnyContent]
}

class AuthenticatedGetArrivalWithoutMessagesForReadActionProviderImpl @Inject()(
  authenticate: AuthenticateActionProvider,
  getArrivalWithoutMessages: AuthenticatedGetArrivalWithoutMessagesActionProvider,
  buildDefault: DefaultActionBuilder
) extends AuthenticatedGetArrivalWithoutMessagesForReadActionProvider {

  def apply(arrivalId: ArrivalId): ActionBuilder[ArrivalWithoutMessagesRequest, AnyContent] =
    buildDefault andThen authenticate() andThen getArrivalWithoutMessages(arrivalId)
}
