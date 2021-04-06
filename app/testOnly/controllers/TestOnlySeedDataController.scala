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

import java.time.Clock

import javax.inject.Inject
import models.ArrivalId
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import testOnly.models.SeedDataParameters
import testOnly.models.SeedDataResponse
import testOnly.models.SeedEori
import testOnly.models.SeedMrn
import testOnly.services.TestOnlySeedDataService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class TestOnlySeedDataController @Inject()(
  override val messagesApi: MessagesApi,
  cc: ControllerComponents,
  clock: Clock,
  repository: ArrivalMovementRepository,
  config: Configuration
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly")

  def seedData: Action[SeedDataParameters] = Action.async(parse.json[SeedDataParameters]) {
    implicit request =>
      if (featureFlag) {
        val startEori         = request.body.startEori
        val numberOfUsers     = request.body.numberOfUsers
        val startMrn          = request.body.startMrn
        val movementsPerUser  = request.body.movementsPerUser
        val startArrivalId    = request.body.startArrivalId
        val maxEori: SeedEori = SeedEori(startEori.prefix, startEori.suffix + numberOfUsers, startEori.padLength)
        val maxMrn: SeedMrn   = SeedMrn(startMrn.prefix, startMrn.suffix + movementsPerUser, startMrn.padLength)

        val totalMovements = numberOfUsers * movementsPerUser
        val endArrivalId   = ArrivalId(startArrivalId.index + totalMovements - 1)

        val response = SeedDataResponse(numberOfUsers, startEori, maxEori, movementsPerUser, startMrn, maxMrn, totalMovements, startArrivalId, endArrivalId)

        dataInsert(request.body).map {
          _ =>
            Ok(Json.toJson(response))
        }
      } else {
        Future.successful(NotImplemented("Feature disabled, could not seed data"))
      }
  }

  private def dataInsert(seedDataParameters: SeedDataParameters): Future[Unit] =
    Future
      .sequence {
        TestOnlySeedDataService
          .seedArrivals(seedDataParameters, clock)
          .map(repository.insert)
      }
      .map(_ => ())

}
