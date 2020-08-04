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
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.FakeAuthenticatedGetArrivalForReadActionProvider
import generators.ModelGenerators
import models.WSError._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.UnloadingPermissionPDFService

import scala.concurrent.Future

class PDFGenerationControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  val mockUnloadingPermissionPDFService: UnloadingPermissionPDFService = mock[UnloadingPermissionPDFService]

  override protected def beforeEach(): Unit = {
    reset(mockUnloadingPermissionPDFService)
    super.beforeEach()
  }

  "PDFGenerationController" - {

    val mockArrivalMovementRepository     = mock[ArrivalMovementRepository]
    val mockUnloadingPermissionPDFService = mock[UnloadingPermissionPDFService]

    "getPDF" - {

      "must return OK and a PDF" in {
        when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(Some(HttpResponse(200, ""))))

        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            val fakeGerArrivalReader = FakeAuthenticatedGetArrivalForReadActionProvider(arrival)
            val application = baseApplicationBuilder
              .overrides(
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService),
                bind[AuthenticatedGetArrivalForReadActionProvider].toInstance(fakeGerArrivalReader)
              )
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual OK
            }
        }
      }

      "must return NotFound when unloading permission is missing" in {
        when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(None))
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            val fakeGerArrivalReader = FakeAuthenticatedGetArrivalForReadActionProvider(arrival)
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(Left(NotFoundError)))

            val application = baseApplicationBuilder
              .overrides(
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService),
                bind[AuthenticatedGetArrivalForReadActionProvider].toInstance(fakeGerArrivalReader)
              )
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual NOT_FOUND
            }
        }
      }

      "must return BadGateway when there is a error in the other microservice" in {

        when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(Some(HttpResponse(500, ""))))
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            val fakeGerArrivalReader = FakeAuthenticatedGetArrivalForReadActionProvider(arrival)
            val application = baseApplicationBuilder
              .overrides(
                bind[AuthenticatedGetArrivalForReadActionProvider].toInstance(fakeGerArrivalReader),
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService)
              )
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual BAD_GATEWAY
            }
        }
      }

      "must return InternalServerError when there is any other errors" in {
        val genErrorCode = Gen.oneOf(300, 499)

        forAll(genArrivalWithSuccessfulArrival, genErrorCode) {
          (arrival, errorCode) =>
            when(mockUnloadingPermissionPDFService.getPDF(any())(any(), any())).thenReturn(Future.successful(Some(HttpResponse(errorCode, ""))))
            val fakeGerArrivalReader = FakeAuthenticatedGetArrivalForReadActionProvider(arrival)

            val application = baseApplicationBuilder
              .overrides(
                bind[AuthenticatedGetArrivalForReadActionProvider].toInstance(fakeGerArrivalReader),
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService)
              )
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
