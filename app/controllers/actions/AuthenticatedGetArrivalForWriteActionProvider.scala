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
import models.ArrivalId
import models.request.ArrivalRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.BodyParsers

import scala.concurrent.ExecutionContext

class AuthenticatedGetArrivalForWriteActionProvider @Inject()(
  lock: LockActionProvider,
  authenticate: AuthenticateActionProvider,
  getArrival: AuthenticatedGetArrivalActionProvider,
  ec: ExecutionContext,
  parser: BodyParsers.Default
) {

  def apply(arrivalId: ArrivalId): ActionBuilder[ArrivalRequest, AnyContent] =
    lock(arrivalId) andThen authenticate() andThen getArrival(arrivalId)
}
