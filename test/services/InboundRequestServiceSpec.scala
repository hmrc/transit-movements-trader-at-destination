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
import models.ArrivalStatus.ArrivalSubmitted
import models.ArrivalStatus.GoodsReleased
import models.ArrivalStatus.UnloadingPermission
import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import models.FailedToUnlock
import models.GoodsReleasedResponse
import models.InboundMessageError
import models.InboundMessageRequest
import models.InvalidArrivalRootNodeError
import models.MessageSender
import models.MovementMessageWithStatus
import models.TransitionError
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running

import scala.concurrent.Future

class InboundRequestServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  val mockLockService       = mock[LockService]
  val mockGetArrivalService = mock[GetArrivalService]

  "InboundRequestService" - {

    "must return an InboundMessageRequest for a valid inbound message" in {

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)
      val message       = arbitrary[MovementMessageWithStatus].sample.value

      val sampleArrival = arbitrary[Arrival].sample.value.copy(
        status = ArrivalSubmitted
      )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Right(sampleArrival)))
      when(mockLockService.unlock(any())).thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        val expectedResult = InboundMessageRequest(sampleArrival, GoodsReleased, GoodsReleasedResponse, message)

        result.futureValue.value mustBe expectedResult
      }
    }

    "must return a DocumentExistsError for an existing lock" in {

      val inboundXml = <CC044A></CC044A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val sampleArrival = arbitrary[Arrival].sample.value.copy(
        status = ArrivalSubmitted
      )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Right(sampleArrival)))
      when(mockLockService.unlock(any())).thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe an[TransitionError]
      }
    }

    "must return an ArrivalNotFoundError when no arrival can be found by arrival Id" in {

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Left(ArrivalNotFoundError("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe ArrivalNotFoundError("error")
      }
    }

    "must return a TransitionError for a request with an invalid transition" in {

      val inboundXml = <CC044A></CC044A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val sampleArrival = arbitrary[Arrival].sample.value.copy(
        status = ArrivalSubmitted
      )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Right(sampleArrival)))
      when(mockLockService.unlock(any())).thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe an[TransitionError]
      }
    }

    "must return an InvalidArrivalRootNode for an unrecognised root node code" in {

      val inboundXml = <InvalidRootCode></InvalidRootCode>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val sampleArrival = arbitrary[Arrival].sample.value.copy(
        status = ArrivalSubmitted
      )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Right(sampleArrival)))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe an[InvalidArrivalRootNodeError]
      }
    }

    "must return an InboundMessageError when given an OutboundMessage" in {

      val unloadingRemarksXml = <CC044A></CC044A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val sampleArrival = arbitrary[Arrival].sample.value.copy(
        status = UnloadingPermission
      )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Right(sampleArrival)))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), unloadingRemarksXml, messageSender)

        result.futureValue.left.value mustBe an[InboundMessageError]
      }
    }

    "must return a FailedToUnlock when unlocking fails" in {

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val sampleArrival = arbitrary[Arrival].sample.value.copy(
        status = ArrivalSubmitted
      )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalById(any())).thenReturn(Future.successful(Right(sampleArrival)))
      when(mockLockService.unlock(any())).thenReturn(Future.successful(Left(FailedToUnlock("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe FailedToUnlock("error")
      }
    }
  }
}
