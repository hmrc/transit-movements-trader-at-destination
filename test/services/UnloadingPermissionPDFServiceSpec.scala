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

package services

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
import play.api.libs.ws.WSResponse
import play.api.libs.ws.ahc.AhcWSResponse
import play.api.libs.ws.ahc.cache.CacheableHttpResponseBodyPart
import play.api.libs.ws.ahc.cache.CacheableHttpResponseStatus
import play.libs.ws.WSResponse
import play.shaded.ahc.org.asynchttpclient.Response
import play.shaded.ahc.org.asynchttpclient.uri.Uri

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UnloadingPermissionPDFServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  "UnloadingPermissionPDFService" - {

    val mockMessageRetrievalService = mock[MessageRetrievalService]
    val mockManageDocumentConnector = mock[ManageDocumentsConnector]

    "getPDF" - {

      "must return Right with a PDF" in {

        val genErrorResponse = Gen.oneOf(300, 500)

        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]], genErrorResponse) {
          (arrival, responseMovementMessage, pdf, errorCode) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            val wsResponse: AhcWSResponse = new AhcWSResponse(
              new Response.ResponseBuilder()
                .accumulate(new CacheableHttpResponseStatus(Uri.create("http://uri"), errorCode, "status text", "protocols!"))
                .build())

            when(mockManageDocumentConnector.getUnloadingPermissionPdf(any())(any()))
              .thenReturn(Future.successful(wsResponse))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            val service = application.injector.instanceOf[UnloadingPermissionPDFService]
            val result  = service.getPDF(arrival).futureValue

            result.left.get mustBe OtherError(errorCode, "")
        }
      }

      "must return Left with OtherError when connector returns another result" in {
        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]]) {
          (arrival, responseMovementMessage, pdf) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            val wsResponse: AhcWSResponse = new AhcWSResponse(
              new Response.ResponseBuilder()
                .accumulate(new CacheableHttpResponseStatus(Uri.create("http://uri"), 200, "status text", "protocols!"))
                .accumulate(new CacheableHttpResponseBodyPart(pdf, true))
                .build())

            when(mockManageDocumentConnector.getUnloadingPermissionPdf(any())(any()))
              .thenReturn(Future.successful(wsResponse))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            val service = application.injector.instanceOf[UnloadingPermissionPDFService]
            val result  = service.getPDF(arrival).futureValue

            result.right.get mustBe pdf
        }
      }

      "must return Left with a NotFoundError when UnloadingPermission is unavailable" in {
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(None)

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .build()

            val service = application.injector.instanceOf[UnloadingPermissionPDFService]
            val result  = service.getPDF(arrival).futureValue

            result.left.get mustBe NotFoundError
        }
      }
    }

  }

}
