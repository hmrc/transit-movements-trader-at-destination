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
import models.Arrival
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
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def seedData: Action[SeedDataParameters] = Action.async(parse.json[SeedDataParameters]) {
    implicit request =>
      val SeedDataParameters(
        startEori,
        numberOfUsers,
        startMrn,
        movementsPerUser
      ) = request.body

      val maxEori: SeedEori = SeedEori(startEori.prefix, startEori.suffix + numberOfUsers, startEori.padLength)
      val maxMrn: SeedMrn   = SeedMrn(startMrn.prefix, startMrn.suffix + movementsPerUser, startMrn.padLength)

      val response = SeedDataResponse(startEori, maxEori, movementsPerUser, startMrn, maxMrn)

      output(request.body).map {
        _ =>
          Ok(Json.toJson(response))
      }
  }

  private def output(seedDataParameters: SeedDataParameters): Future[Unit] =
    Future
      .sequence {
        TestOnlySeedDataService
          .seedArrivals(seedDataParameters, clock)
          .map(repository.insert)
      }
      .map(_ => ())

}
