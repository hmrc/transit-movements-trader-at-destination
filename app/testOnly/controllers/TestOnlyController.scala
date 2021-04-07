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

import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection.JSONCollection
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class TestOnlyController @Inject()(
  override val messagesApi: MessagesApi,
  mongo: ReactiveMongoApi,
  cc: ControllerComponents,
  config: Configuration
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  def dropArrivalMovementCollection: Action[AnyContent] = Action.async {
    implicit request =>
      if (featureFlag) {
        mongo.database
          .map(_.collection[JSONCollection](ArrivalMovementRepository.collectionName))
          .flatMap(
            _.drop(failIfNotFound = false) map {
              case true  => Ok(s"Dropped  '${ArrivalMovementRepository.collectionName}' Mongo collection")
              case false => Ok("collection does not exist or something gone wrong")
            }
          )
      } else {
        Future.successful(NotImplemented(s"Feature disabled, cannot drop ${ArrivalMovementRepository.collectionName}"))
      }
  }

}
