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

package controllers.testOnly

import java.time.Clock

import javax.inject.Inject
import models.Arrival
import models.testOnly.SeedDataParameters
import models.testOnly.SeedDataResponse
import models.testOnly.SeedEori
import models.testOnly.SeedMrn
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services.testOnly.TestOnlySeedDataService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext

class TestOnlySeedDataController @Inject()(override val messagesApi: MessagesApi, cc: ControllerComponents, clock: Clock)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def seedData: Action[SeedDataParameters] = Action(parse.json[SeedDataParameters]) {
    implicit request =>
      Ok(Json.toJson(output(request.body)))
  }

  private def output(seedDataParameters: SeedDataParameters): SeedDataResponse = {
    val SeedDataParameters(
      startEori,
      numberOfUsers,
      startMrn,
      movementsPerUser
    ) = seedDataParameters

    val dataToInsert: Iterator[Arrival] = TestOnlySeedDataService.seedArrivals(seedDataParameters, clock) // TODO: Use this

    val maxEori: SeedEori = SeedEori(startEori.prefix, startEori.suffix + numberOfUsers, startEori.padLength)
    val maxMrn: SeedMrn   = SeedMrn(startMrn.prefix, startMrn.suffix + movementsPerUser, startMrn.padLength)

    SeedDataResponse(startEori, maxEori, movementsPerUser, startMrn, maxMrn)
  }
}
