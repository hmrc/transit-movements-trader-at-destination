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
import models.WSError._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import services.UnloadingPermissionPDFService

import scala.concurrent.Future

class PDFGenerationControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with IntegrationPatience {

  "PDFGenerationController" - {

    val mockArrivalMovementRepository     = mock[ArrivalMovementRepository]
    val mockUnloadingPermissionPDFService = mock[UnloadingPermissionPDFService]

    "getPDF" - {

      "must return OK and a PDF" in {
        forAll(genArrivalWithSuccessfulArrival, arbitrary[Array[Byte]]) {
          (arrival, pdf) =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any()))
              .thenReturn(Future.successful(Right(pdf)))

            val application = baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .overrides(bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService))
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual OK
            }
        }
      }

      "must return NotFound when unloading permission is missing" in {
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(Left(NotFoundError)))

            val application = baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .overrides(bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService))
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual NOT_FOUND
            }
        }
      }

      "must return BadGateway for other errors" in {

        val genErrorCode = Gen.oneOf(300, 500).sample.value

        forAll(genArrivalWithSuccessfulArrival, genErrorCode) {
          (arrival, errorCode) =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(Left(OtherError(errorCode, ""))))

            val application = baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .overrides(bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService))
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual BAD_GATEWAY
            }
        }
      }
    }
  }

}
