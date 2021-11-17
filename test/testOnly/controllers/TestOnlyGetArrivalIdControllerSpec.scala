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

import base._
import generators.ModelGenerators
import models.ArrivalId
import org.mockito.Mockito
import org.mockito.Mockito.when
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsNumber
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalIdRepository
import repositories.ArrivalMovementRepository

import java.time.Clock
import java.time.Instant
import java.time.ZoneId
import scala.concurrent.Future

class TestOnlyGetArrivalIdControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with GuiceOneAppPerSuite with BeforeAndAfterEach {

  val mockRepository = mock[ArrivalMovementRepository]

  override def beforeEach: Unit =
    Mockito.reset(mockRepository)

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure("play.http.router" -> "testOnlyDoNotUseInAppConf.Routes", "feature-flags.testOnly.enabled" -> true)
      .overrides(bind[Clock].toInstance(Clock.fixed(Instant.now(), ZoneId.systemDefault)), bind[ArrivalMovementRepository].toInstance(mockRepository))
      .build()

  "getLatestArrivalId" - {

    "must return Ok with latest ArrivalId" in {

      when(mockRepository.getMaxArrivalId).thenReturn(Future.successful(Some(ArrivalId(123))))

      val request = FakeRequest(GET, testOnly.controllers.routes.TestOnlyGetArrivalIdController.getLatestArrivalId().url)

      val result = route(app, request).value

      status(result) mustEqual OK
      contentAsJson(result) mustEqual JsNumber(123)
    }

    "must return NotFound when no ArrivalId's are available" in {

      when(mockRepository.getMaxArrivalId).thenReturn(Future.successful(None))

      val request = FakeRequest(GET, testOnly.controllers.routes.TestOnlyGetArrivalIdController.getLatestArrivalId().url)

      val result = route(app, request).value

      status(result) mustEqual NOT_FOUND
    }
  }

}
