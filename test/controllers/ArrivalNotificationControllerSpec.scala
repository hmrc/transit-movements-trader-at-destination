/*
 * Copyright 2019 HM Revenue & Customs
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

package controllers

import generators.MessageGenerators
import models.messages.NormalNotification
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.SubmissionService
import services.mocks.MockSubmissionService

class ArrivalNotificationControllerSpec extends
  FreeSpec with
  MustMatchers with
  ScalaCheckPropertyChecks with
  MessageGenerators with
  GuiceOneAppPerSuite with
  OptionValues with
  MockSubmissionService {

  /**
    * SHOULD
    * Return 200 on successful conversion to ArrivalNotification
    * Return 400 when ArrivalNotification could not be built
    * Return 401 when user isn't authenticated
    * Return 502 if EIS is down
    * Return 504 (check this doesn't happen automatically
    */

  override implicit lazy val app = new GuiceApplicationBuilder()
    .overrides(
      bind[SubmissionService].toInstance(mockSubmissionService)
    ).build()

  "post" - {

    "must return BAD_REQUEST when can't be converted to ArrivalNotification" in {

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.obj("key" -> "value"))
      val result = route(app, request).value

      status(result) mustEqual BAD_REQUEST
    }

    "must return BAD_GATEWAY when the EIS service is down" in {

      for (
        normalNotification <- arbitrary[NormalNotification]
      ) yield {
        mockSubmit(502, normalNotification)

        val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
          .withJsonBody(Json.toJson(normalNotification))
        val result = route(app, request).value

        status(result) mustEqual BAD_GATEWAY
      }
    }
  }

  "must return OK when passed valid NormalNotification" in {

    for (
      normalNotification <- arbitrary[NormalNotification]
    ) yield {
      mockSubmit(200, normalNotification)

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))
      val result = route(app, request).value

      status(result) mustEqual OK
    }
  }

}
