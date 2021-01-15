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

import java.time.LocalDate
import java.time.LocalTime

import audit.AuditService
import base.SpecBase
import models.ArrivalStatus._
import models.ArrivalId
import models.ChannelType
import models.GoodsReleasedResponse
import models.MessageSender
import models.SubmissionProcessingResult
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SaveMessageServiceSpec extends SpecBase with BeforeAndAfterEach {

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

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService),
          bind[AuditService].toInstance(mockAuditService)
        )
        .build()

      running(application) {
        val saveMessageService = application.injector.instanceOf[SaveMessageService]

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val arrivalId            = ArrivalId(1)
        val messageCorrelationId = 1
        val messageSender        = MessageSender(arrivalId, messageCorrelationId)
        val channel              = ChannelType.web

        val requestGoodsReleasedXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val result =
          saveMessageService.validateXmlAndSaveMessage(requestGoodsReleasedXmlBody, messageSender, GoodsReleasedResponse, GoodsReleased, channel).futureValue

        result mustBe SubmissionProcessingResult.SubmissionSuccess
        verify(mockArrivalMovementRepository, times(1)).addResponseMessage(eqTo(arrivalId), any(), eqTo(GoodsReleased))
        verify(mockXmlValidationService, times(1)).validate(any(), any())
        verify(mockAuditService, times(1)).auditNCTSMessages(any(), any(), any())(any())
      }
    }

    "return Failure when we cannot save the message" in {
      when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Failure(new Exception)))
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      running(application) {
        val saveMessageService = application.injector.instanceOf[SaveMessageService]

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val arrivalId            = ArrivalId(1)
        val messageCorrelationId = 1
        val messageSender        = MessageSender(arrivalId, messageCorrelationId)
        val channel              = ChannelType.api

        val requestGoodsReleasedXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val result =
          saveMessageService.validateXmlAndSaveMessage(requestGoodsReleasedXmlBody, messageSender, GoodsReleasedResponse, GoodsReleased, channel).futureValue

        result mustBe SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockArrivalMovementRepository, times(1)).addResponseMessage(any(), any(), any())
        verify(mockXmlValidationService, times(1)).validate(any(), any())
      }
    }

    "return Failure when we cannot parse the message" in {
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Failure(new Exception))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      running(application) {
        val saveMessageService = application.injector.instanceOf[SaveMessageService]

        val arrivalId            = ArrivalId(1)
        val messageCorrelationId = 1
        val messageSender        = MessageSender(arrivalId, messageCorrelationId)
        val channel              = ChannelType.web

        val requestInvalidXmlBody = <Invalid>invalid</Invalid>

        val result =
          saveMessageService.validateXmlAndSaveMessage(requestInvalidXmlBody, messageSender, GoodsReleasedResponse, GoodsReleased, channel).futureValue

        result mustBe SubmissionProcessingResult.SubmissionFailureExternal
        verify(mockArrivalMovementRepository, never()).addResponseMessage(any(), any(), any())
        verify(mockXmlValidationService, times(1)).validate(any(), any())
      }
    }

    "return Failure when we cannot parse the message due malformed time" in {
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      running(application) {
        val saveMessageService = application.injector.instanceOf[SaveMessageService]

        val arrivalId            = ArrivalId(1)
        val messageCorrelationId = 1
        val messageSender        = MessageSender(arrivalId, messageCorrelationId)
        val dateOfPrep           = LocalDate.now()
        val timeOfPrep           = LocalTime.of(1, 1)
        val channel              = ChannelType.api

        val requestInvalidXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) + "/"}</TimOfPreMES10>
          </CC025A>

        val result =
          saveMessageService.validateXmlAndSaveMessage(requestInvalidXmlBody, messageSender, GoodsReleasedResponse, GoodsReleased, channel).futureValue

        result mustBe SubmissionProcessingResult.SubmissionFailureExternal
        verify(mockArrivalMovementRepository, never()).addResponseMessage(any(), any(), any())
        verify(mockXmlValidationService, times(1)).validate(any(), any())
      }
    }
  }
}
