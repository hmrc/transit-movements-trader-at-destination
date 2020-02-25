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
import models.ArrivalMovement
import models.Message
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories.ArrivalMovementRepository
import play.api.inject.bind
import org.mockito.Mockito.reset
import org.mockito.Mockito.when
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future

class ArrivalsHistoryControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach with IntegrationPatience {

  private val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

  private val application = {
    applicationBuilder
      .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
      .build
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockArrivalMovementRepository)
  }

  "ArrivalsHistoryController" - {

    "must return Ok and retrieve arrivals history" in {

      forAll(seqWithMaxLength[ArrivalMovement](10)) {
        arrivalMovements =>
          when(mockArrivalMovementRepository.fetchAllMovements)
            .thenReturn(Future.successful(arrivalMovements))

          val expectedResult: Seq[Message] = arrivalMovements.map(_.messages.head)

          val request = FakeRequest(GET, routes.ArrivalsHistoryController.getArrivalsHistory().url)
            .withJsonBody(Json.toJson(expectedResult))

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedResult)
      }

    }

    "must return an INTERNAL_SERVER_ERROR on fail" in {

      when(mockArrivalMovementRepository.fetchAllMovements)
        .thenReturn(Future.failed(new Exception))

      val request = FakeRequest(GET, routes.ArrivalsHistoryController.getArrivalsHistory().url)

      val result = route(application, request).value

      status(result) mustEqual INTERNAL_SERVER_ERROR
    }

  }
}
