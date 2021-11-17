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

package testOnly.controllers

import logging.Logging
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class TestOnlyGetArrivalIdController @Inject()(
  override val messagesApi: MessagesApi,
  cc: ControllerComponents,
  repository: ArrivalMovementRepository,
  config: Configuration,
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  def getLatestArrivalId: Action[AnyContent] = Action.async {
    implicit request =>
      if (featureFlag) {
        repository.getMaxArrivalId.map {
          case Some(arrivalId) => Ok(Json.toJson(arrivalId))
          case None            => NotFound
        }
      } else {
        Future.successful(NotImplemented("Feature disabled, could not retrieve ArrivalId"))
      }
  }

}
