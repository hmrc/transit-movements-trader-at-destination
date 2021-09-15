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

import audit.AuditService
import base.SpecBase
import generators.ModelGenerators
import models.ArrivalStatus._
import models.ArrivalId
import models.ArrivalWithoutMessages
import models.FailedToSaveMessage
import models.GoodsReleasedResponse
import models.InboundMessageRequest
import models.MessageSender
import models.MovementMessageWithoutStatus
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SaveMessageServiceSpec extends SpecBase with BeforeAndAfterEach with ModelGenerators {

  private val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
  private val mockXmlValidationService      = mock[XmlValidationService]
  private val mockAuditService              = mock[AuditService]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockArrivalMovementRepository)
    reset(mockXmlValidationService)
    reset(mockAuditService)
  }

  "validateXmlAndSaveMessage" - {

    "must audit the call and returns Success when we successfully save a message" in {
      when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val arrival = arbitrary[ArrivalWithoutMessages].sample.value
      val message = arbitrary[MovementMessageWithoutStatus].sample.value

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService),
          bind[AuditService].toInstance(mockAuditService)
        )
        .build()

      running(application) {
        val saveMessageService = application.injector.instanceOf[SaveMessageService]

        val arrivalId            = ArrivalId(1)
        val messageCorrelationId = 1
        val messageSender        = MessageSender(arrivalId, messageCorrelationId)

        val result: Unit =
          saveMessageService
            .saveInboundMessage(InboundMessageRequest(arrival, GoodsReleased, GoodsReleasedResponse, message), messageSender)
            .futureValue
            .value

        result mustBe (())
        verify(mockArrivalMovementRepository, times(1)).addResponseMessage(eqTo(arrivalId), any(), eqTo(GoodsReleased))
        verify(mockAuditService, times(1)).auditNCTSMessages(any(), eqTo(arrival.eoriNumber), any(), any())(any())
      }
    }

    "return Failure when we cannot save the message" in {

      when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Failure(new Exception)))

      val message = arbitrary[MovementMessageWithoutStatus].sample.value
      val arrival = arbitrary[ArrivalWithoutMessages].sample.value

      val application = baseApplicationBuilder.overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)).build()

      running(application) {
        val saveMessageService = application.injector.instanceOf[SaveMessageService]

        val arrivalId            = ArrivalId(1)
        val messageCorrelationId = 1
        val messageSender        = MessageSender(arrivalId, messageCorrelationId)

        val result =
          saveMessageService
            .saveInboundMessage(InboundMessageRequest(arrival, GoodsReleased, GoodsReleasedResponse, message), messageSender)
            .futureValue
            .left
            .value

        result mustBe an[FailedToSaveMessage]
        verify(mockArrivalMovementRepository, times(1)).addResponseMessage(any(), any(), any())
      }
    }
  }
}
