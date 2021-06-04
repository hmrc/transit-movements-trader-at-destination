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

package services

import akka.util.ByteString
import base.SpecBase
import connectors.ManageDocumentsConnector
import generators.ModelGenerators
import models.WSError.NotFoundError
import models.WSError.OtherError
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.libs.ws.ahc.AhcWSResponse
import play.api.test.Helpers.CONTENT_DISPOSITION
import play.api.test.Helpers.CONTENT_TYPE
import play.api.test.Helpers.running

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UnloadingPermissionPDFServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  "UnloadingPermissionPDFService" - {

    val mockMessageRetrievalService   = mock[MessageRetrievalService]
    val mockManageDocumentConnector   = mock[ManageDocumentsConnector]
    val mockWSResponse: AhcWSResponse = mock[AhcWSResponse]

    "getPDF" - {

      "must return Left with OtherError when connector returns another result" in {

        val genErrorResponse = Gen.oneOf(300, 500)

        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]], genErrorResponse) {
          (arrival, responseMovementMessage, pdf, errorCode) =>
            val expectedHeaders = Map(CONTENT_TYPE -> Seq("application/pdf"), CONTENT_DISPOSITION -> Seq("unloading_permission_123"))

            when(mockWSResponse.status) thenReturn errorCode
            when(mockWSResponse.body) thenReturn "Error message"
            when(mockWSResponse.headers) thenReturn expectedHeaders

            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            when(mockManageDocumentConnector.getUnloadingPermissionPdf(any())(any()))
              .thenReturn(Future.successful(mockWSResponse))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.left.get mustBe OtherError(errorCode, "Error message")
            }
        }
      }

      "must return Right with a PDF" in {
        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]]) {
          (arrival, responseMovementMessage, pdf) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            val headers = Map(CONTENT_TYPE -> Seq("application/pdf"), CONTENT_DISPOSITION -> Seq("unloading_permission_123"))

            when(mockWSResponse.status) thenReturn 200
            when(mockWSResponse.bodyAsBytes) thenReturn ByteString(pdf)
            when(mockWSResponse.headers) thenReturn headers

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              val expectedHeaders: Seq[(String, String)] = Seq((CONTENT_TYPE, "application/pdf"), (CONTENT_DISPOSITION, "unloading_permission_123"))

              result.right.get._1 mustBe pdf
              result.right.get._2 mustBe expectedHeaders
            }
        }
      }

      "must return Left with a NotFoundError when UnloadingPermission is unavailable" in {
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(None)

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.left.get mustBe NotFoundError
            }
        }
      }
    }
  }
}
