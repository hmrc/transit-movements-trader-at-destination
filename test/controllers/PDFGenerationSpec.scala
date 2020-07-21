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
import generators.ModelGenerators
import models.Arrival
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import services.MessageRetrievalService

import scala.concurrent.Future

class PDFGenerationSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators {

  "PDFGenerationController" - {

    val mockMessageRetrievalService   = mock[MessageRetrievalService]
    val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

    "post" - {

      "must return OK and a PDF" in {

        forAll(arbitrary[ResponseMovementMessage], arbitrary[Arrival]) {
          (responseMovementMessage, arrival) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            when(mockArrivalMovementRepository.get(any()))
              .thenReturn(Future.successful(Some(arrival)))

            val application =
              baseApplicationBuilder
                .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
                .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
                .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.post(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual OK
              contentAsString(result) mustEqual responseMovementMessage.message.toString
            }
        }
      }

      "must return NotFound when unloading permission is missing" in {

        forAll(arbitrary[Arrival]) {
          arrival =>
            when(mockArrivalMovementRepository.get(any()))
              .thenReturn(Future.successful(Some(arrival)))

            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(None)

            val application =
              baseApplicationBuilder
                .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
                .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
                .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.post(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual NOT_FOUND
            }
        }
      }
    }
  }

}
