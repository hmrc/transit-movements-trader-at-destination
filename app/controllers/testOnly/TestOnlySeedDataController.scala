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
      Ok(Json.toJson(request.body))
  }

  private def output(seedDataParameters: SeedDataParameters): String =
    //  eoriRangeStart: "ZZ0000001",
    //  eoriRangeEnd: "ZZ0000101",
    //  movementsPerUser: 10,
    //  startMrn: 2100000000,
    //  endMrn: 2100000010,
    ???

}
