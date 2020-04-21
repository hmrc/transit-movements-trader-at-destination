/*
 * Copyright 2020 HM Revenue & Customs
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

import base.SpecBase
import generators.MessageGenerators
import models.Arrival
import models.Arrivals
import org.mockito.Matchers.any
import org.mockito.Mockito.reset
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository

import scala.concurrent.Future

class GetArrivalsControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach with IntegrationPatience {

  "getArrivals" - {

    "must return Ok with the retrieved arrivals" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      val application =
        baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

      running(application) {
        forAll(seqWithMaxLength[Arrival](10)) {
          arrivals =>
            when(mockArrivalMovementRepository.fetchAllArrivals(any())).thenReturn(Future.successful(arrivals))

            val request = FakeRequest(GET, routes.GetArrivalsController.get().url)

            val result = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result) mustEqual Json.toJson(Arrivals(arrivals))

            reset(mockArrivalMovementRepository)
        }
      }
    }

    "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the Arrival Movements" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      when(mockArrivalMovementRepository.fetchAllArrivals(any()))
        .thenReturn(Future.failed(new Exception))

      val application =
        baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

      running(application) {

        val request = FakeRequest(GET, routes.GetArrivalsController.get().url)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }

}
