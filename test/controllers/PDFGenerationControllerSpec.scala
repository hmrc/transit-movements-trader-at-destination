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
import cats.data.NonEmptyList
import connectors.ManageDocumentsConnector
import generators.ModelGenerators
import models.MessageStatus.SubmissionSucceeded
import models.Arrival
import models.MovementMessageWithStatus
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import services.MessageRetrievalService
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.Future

class PDFGenerationControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with IntegrationPatience {

  "PDFGenerationController" - {

    val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
    val mockMessageRetrievalService   = mock[MessageRetrievalService]
    val mockManageDocumentsConnector  = mock[ManageDocumentsConnector]

    val genArrival = {
      Arbitrary {
        for {
          message <- Arbitrary.arbitrary[MovementMessageWithStatus]
          arrival <- Arbitrary.arbitrary[Arrival]
        } yield {
          val successfulMessage = message.copy(status = SubmissionSucceeded)
          arrival.copy(messages = NonEmptyList.one(successfulMessage), eoriNumber = "eori")
        }
      }.arbitrary
    }

    val genErrorCode = Gen.oneOf(300, 599)

    "post" - {

      "must return OK and a PDF" in {
        forAll(genArrival, Arbitrary.arbitrary[ResponseMovementMessage]) {
          (arrival, responseMovementMessage) =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(Some(responseMovementMessage))
            when(mockManageDocumentsConnector.getUnloadingPermissionPdf(any())(any())).thenReturn(Future.successful(HttpResponse(200, "")))

            val application = baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentsConnector))
              .build()

            running(application) {
              val request = FakeRequest(POST, routes.PDFGenerationController.post(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual OK
            }
        }
      }

      "must return NotFound when unloading permission is missing" in {
        forAll(genArrival) {
          arrival =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(None)

            val application = baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .build()

            running(application) {
              val request = FakeRequest(POST, routes.PDFGenerationController.post(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual NOT_FOUND
            }
        }
      }

      "must return BadRequest when there is any other errors" in {
        forAll(genArrival, Arbitrary.arbitrary[ResponseMovementMessage], genErrorCode) {
          (arrival, responseMovementMessage, errorCode) =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(Some(responseMovementMessage))
            when(mockManageDocumentsConnector.getUnloadingPermissionPdf(any())(any())).thenReturn(Future.successful(HttpResponse(errorCode, "")))

            val application = baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentsConnector))
              .build()

            running(application) {
              val request = FakeRequest(POST, routes.PDFGenerationController.post(arrival.arrivalId).url)
              val result  = route(application, request).value

              status(result) mustEqual BAD_REQUEST
            }
        }
      }
    }
  }

}
