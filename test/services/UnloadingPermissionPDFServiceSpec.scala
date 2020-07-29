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
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UnloadingPermissionPDFServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  "UnloadingPermissionPDFService" - {

    val mockMessageRetrievalService = mock[MessageRetrievalService]
    val mockManageDocumentConnector = mock[ManageDocumentsConnector]

    "getPDF" - {

      "must return a successful HttpResponse" in {
        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]]) {
          (arrival, responseMovementMessage, pdf) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            when(mockManageDocumentConnector.getUnloadingPermissionPdf(any())(any()))
              .thenReturn(Future.successful(pdf))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            val service = application.injector.instanceOf[UnloadingPermissionPDFService]
            val result  = service.getPDF(arrival).futureValue.value

            result mustBe pdf
        }
      }

      "must return None when UnloadingPermission cannot be found" in {
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(None)

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .build()

            val service = application.injector.instanceOf[UnloadingPermissionPDFService]
            val result  = service.getPDF(arrival).futureValue

            result mustBe None
        }
      }
    }

  }

}
