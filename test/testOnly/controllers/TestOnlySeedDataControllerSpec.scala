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
import java.time.Instant
import java.time.ZoneId
import base._
import generators.ModelGenerators
import models.ChannelType
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsJson
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository

import scala.concurrent.Future

class TestOnlySeedDataControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with GuiceOneAppPerSuite with BeforeAndAfterEach {

  val mockRepository = mock[ArrivalMovementRepository]

  override def beforeEach: Unit =
    Mockito.reset(mockRepository)

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure("play.http.router" -> "testOnlyDoNotUseInAppConf.Routes")
      .overrides(
        bind[Clock].toInstance(Clock.fixed(Instant.now(), ZoneId.systemDefault)),
        bind[ArrivalMovementRepository].toInstance(mockRepository),
      )
      .build()

  "seedData" - {

    "must return OK, when the service validates and save the message" in {

      when(mockRepository.insert(any())).thenReturn(Future.successful(()))

      val request: FakeRequest[AnyContentAsJson] = FakeRequest(POST, testOnly.controllers.routes.TestOnlySeedDataController.seedData().url)
        .withHeaders("channel" -> ChannelType.web.toString)
        .withJsonBody(Json.parse("""
            |{
            |  "numberOfUsers": 100,
            |  "movementsPerUser": 10
            |}""".stripMargin).as[JsObject])

      val result = route(app, request).value

      status(result) mustEqual OK
      contentAsJson(result) mustEqual Json.obj(
        "eoriRangeStart"         -> "ZZ000000000001",
        "eoriRangeEnd"           -> "ZZ000000000101",
        "numberOfUsers"          -> 100,
        "mrnRangeStart"          -> "21GB00000000000001",
        "mrnRangeEnd"            -> "21GB00000000000011",
        "movementsPerUser"       -> 10,
        "totalInsertedMovements" -> 1000
      )

      verify(mockRepository, times(1000)).insert(any())
    }
  }
}
