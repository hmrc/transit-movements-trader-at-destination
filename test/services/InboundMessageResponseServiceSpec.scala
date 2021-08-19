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

import base.SpecBase
import generators.ModelGenerators
import models.CannotFindRootNodeError
import models.FailedToValidateMessage
import models.GoodsReleasedResponse
import models.InboundMessageError
import models.InvalidArrivalRootNodeError
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running

import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class InboundMessageResponseServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockXmlValidationService = mock[XmlValidationService]

  "InboundMessageResponseService" - {

    "makeInboundMessageResponse" - {

      "must return an InboundMessageResponse" in {

        val xml = <CC025A></CC025A>

        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[InboundMessageResponseService]

          val result = service.makeInboundMessageResponse(xml).value

          result.futureValue.value mustBe GoodsReleasedResponse
        }
      }

      "must return a CannotFindRootNodeError when root node is missing" in {

        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[InboundMessageResponseService]

          val result = service.makeInboundMessageResponse(NodeSeq.Empty).value

          result.futureValue.left.value mustBe an[CannotFindRootNodeError]
        }
      }

      "must return an InvalidArrivalRootNodeError when root node is invalid" in {

        val xml = <invalid></invalid>

        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[InboundMessageResponseService]

          val result = service.makeInboundMessageResponse(xml).value

          result.futureValue.left.value mustBe an[InvalidArrivalRootNodeError]
        }
      }

      "must return an InboundMessageError when message is an Outbound message" in {

        val unloadingRemarksXml = <CC044A></CC044A>

        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[InboundMessageResponseService]

          val result = service.makeInboundMessageResponse(unloadingRemarksXml).value

          result.futureValue.left.value mustBe an[InboundMessageError]
        }
      }

      "must return an FailedToValidateMessage when xml fails to validate against XSD" in {

        val xml = <CC025A></CC025A>

        when(mockXmlValidationService.validate(any(), any())).thenReturn(Failure((new Exception)))

        val application = baseApplicationBuilder
          .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[InboundMessageResponseService]

          val result = service.makeInboundMessageResponse(xml).value

          result.futureValue.left.value mustBe an[FailedToValidateMessage]
        }
      }
    }
  }
}
