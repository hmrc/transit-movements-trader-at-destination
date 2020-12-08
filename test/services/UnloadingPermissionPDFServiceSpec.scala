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

import akka.util.ByteString
import base.SpecBase
import connectors.ManageDocumentsConnector
import generators.ModelGenerators
import models.WSError.{NotFoundError, OtherError}
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.libs.ws.ahc.AhcWSResponse
import play.api.test.Helpers.running

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UnloadingPermissionPDFServiceSpec extends SpecBase with BeforeAndAfterEach with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockMessageRetrievalService = mock[MessageRetrievalService]
  private val mockManageDocumentConnector = mock[ManageDocumentsConnector]
  private val mockWsrResponse             = mock[AhcWSResponse]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockMessageRetrievalService)
    reset(mockManageDocumentConnector)
    reset(mockWsrResponse)
  }

  "UnloadingPermissionPDFService" - {

    "getPDF" - {

      "must return Left with OtherError when connector returns a result other than OK" in {

        val genErrorResponse = Gen.oneOf(300, 500)

        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], genErrorResponse) {
          (arrival, responseMovementMessage, errorCode) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            when(mockWsrResponse.status) thenReturn errorCode
            when(mockWsrResponse.body) thenReturn "foo"

            when(mockManageDocumentConnector.getUnloadingPermissionPdf(any())(any()))
              .thenReturn(Future.successful(mockWsrResponse))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.left.get mustBe OtherError(errorCode, "foo")
            }
        }
      }

      "must return Right with a PDF when the connector returns OK with a PDF" in {
        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]]) {
          (arrival, responseMovementMessage, pdf) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            when(mockWsrResponse.status) thenReturn 200
            when(mockWsrResponse.bodyAsBytes) thenReturn ByteString(pdf)

            when(mockManageDocumentConnector.getUnloadingPermissionPdf(any())(any()))
              .thenReturn(Future.successful(mockWsrResponse))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.right.get mustBe pdf
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
