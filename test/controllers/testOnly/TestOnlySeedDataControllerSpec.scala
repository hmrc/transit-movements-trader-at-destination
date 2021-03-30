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

import base._
import generators.ModelGenerators
import models.ChannelType
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsJson
import play.api.test.FakeRequest
import play.api.test.Helpers._

class TestOnlySeedDataControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with GuiceOneAppPerSuite {

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure("play.http.router" -> "testOnlyDoNotUseInAppConf.Routes")
      .build()

  "seedData" - {

    "must return OK, when the service validates and save the message" in {

      val request: FakeRequest[AnyContentAsJson] = FakeRequest(POST, controllers.testOnly.routes.TestOnlySeedDataController.seedData().url)
        .withHeaders("channel" -> ChannelType.web.toString)
        .withJsonBody(Json.parse("""
            |{
            |  "startEori": "ZZ0000001",
            |  "numberOfUsers": 100,
            |  "startMrn": "21GB00000000000000",
            |  "movementsPerUser": 10
            |}""".stripMargin).as[JsObject])

      val result = route(app, request).value

      status(result) mustEqual OK
      contentAsJson(result) mustEqual Json.obj(
        "eoriStart"        -> "ZZ0000001",
        "eoriEnd"          -> "ZZ0000101",
        "movementsPerUser" -> 10,
        "startMrn"         -> "21GB00000000000000",
        "endMrn"           -> "21GB00000000000010",
      )
    }
  }

}
