/*
 * Copyright 2023 HM Revenue & Customs
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

import akka.stream.scaladsl.Source
import akka.util.ByteString
import base.SpecBase
import generators.ModelGenerators
import models.MovementMessage
import models.PdfDocument
import models.WSError._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
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

        forAll(
          genArrivalMessagesWithSpecificEori,
          arbitrary[Array[Byte]],
          arbitrary[Option[Long]],
          arbitrary[Option[String]],
          arbitrary[Option[String]]
        ) {
          (arrivalMessages, pdf, contentLength, contentType, contentDisposition) =>
            when(mockUnloadingPermissionPDFService.getPDF(any[List[MovementMessage]])(any(), any()))
              .thenReturn(Future.successful(Right(PdfDocument(Source.single(ByteString(pdf)), contentLength, contentType, contentDisposition))))

            when(mockArrivalMovementRepository.getMessagesOfType(refEq(arrivalMessages.arrivalId), any(), any()))
              .thenReturn(Future.successful(Some(arrivalMessages)))

            val application = baseApplicationBuilder
              .overrides(
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService),
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
              )
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrivalMessages.arrivalId).url)
                .withHeaders("channel" -> "api")

              val result = route(application, request).value

              status(result) mustEqual OK
            }
        }
      }

      "must return NotFound when unloading permission is missing" in {
        forAll(genArrivalMessagesWithSpecificEori) {
          arrivalMessages =>
            when(mockUnloadingPermissionPDFService.getPDF(any[List[MovementMessage]])(any(), any()))
              .thenReturn(Future.successful(Left(NotFoundError)))

            when(mockArrivalMovementRepository.getMessagesOfType(refEq(arrivalMessages.arrivalId), any(), any()))
              .thenReturn(Future.successful(Some(arrivalMessages)))

            val application = baseApplicationBuilder
              .overrides(
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService),
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
              )
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrivalMessages.arrivalId).url)
                .withHeaders("channel" -> "api")

              val result = route(application, request).value

              status(result) mustEqual NOT_FOUND
            }
        }
      }

      "must return BadGateway when there is any other errors" in {
        val genErrorCode = Gen.oneOf(300, 499)

        forAll(genArrivalMessagesWithSpecificEori, genErrorCode) {
          (arrivalMessages, errorCode) =>
            when(mockUnloadingPermissionPDFService.getPDF(any[List[MovementMessage]])(any(), any()))
              .thenReturn(Future.successful(Left(OtherError(errorCode, ""))))

            when(mockArrivalMovementRepository.getMessagesOfType(refEq(arrivalMessages.arrivalId), any(), any()))
              .thenReturn(Future.successful(Some(arrivalMessages)))

            val application = baseApplicationBuilder
              .overrides(
                bind[UnloadingPermissionPDFService].toInstance(mockUnloadingPermissionPDFService),
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
              )
              .build()

            running(application) {
              val request = FakeRequest(GET, routes.PDFGenerationController.getPDF(arrivalMessages.arrivalId).url)
                .withHeaders("channel" -> "api")

              val result = route(application, request).value

              status(result) mustEqual BAD_GATEWAY
            }
        }
      }
    }
  }

}
