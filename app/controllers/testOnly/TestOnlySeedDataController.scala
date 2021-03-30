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

import javax.inject.Inject
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext

class TestOnlySeedDataController @Inject()(override val messagesApi: MessagesApi, cc: ControllerComponents)(implicit ec: ExecutionContext)
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

    // Eori range
    val eoriCount          = startEori.substring(2) + numberOfUsers
    val eoriPrefix: String = startEori.slice(0, 1)
    val endEori            = eoriPrefix + eoriCount

    // MRN range
    val mrnPrefix = startMrn.substring(0, 4)
    val mrnSuffix = startMrn.substring(4, 18)

    val range: Seq[Int] = 1 to movementsPerUser

    val replaceMrn = range.map {
      x =>
        val indexOfStringToTrim = mrnSuffix.length - (x.toString.length - 1)
        mrnPrefix + mrnSuffix.substring(0, indexOfStringToTrim) + x
    }

    SeedDataResponse(startEori, endEori, movementsPerUser, startMrn, replaceMrn.last)
  }

}
