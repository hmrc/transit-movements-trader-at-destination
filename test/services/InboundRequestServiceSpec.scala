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

package services

import base.SpecBase
import cats.data.EitherT
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.ArrivalWithoutMessages
import models.DocumentExistsError
import models.FailedToUnlock
import models.GoodsReleasedResponse
import models.InboundMessageRequest
import models.InboundMessageResponse
import models.MessageSender
import models.MovementMessageWithoutStatus
import models.SubmissionState
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running

import scala.concurrent.Future

class InboundRequestServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  val mockLockService                   = mock[LockService]
  val mockGetArrivalService             = mock[GetArrivalService]
  val mockInboundMessageResponseService = mock[InboundMessageResponseService]
  val mockMovementMessage               = mock[MovementMessageService]

  "InboundRequestService" - {

    "must return an InboundMessageRequest for a valid inbound message" in {

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)
      val sampleArrival = arbitrary[ArrivalWithoutMessages].sample.value

      val message = arbitrary[MovementMessageWithoutStatus].sample.value.copy(messageId = sampleArrival.nextMessageId)

      val movementMessage: EitherT[Future, SubmissionState, MovementMessageWithoutStatus] =
        EitherT[Future, SubmissionState, MovementMessageWithoutStatus](
          Future.successful(Right(message))
        )

      val inboundMessageResponse: EitherT[Future, SubmissionState, InboundMessageResponse] =
        EitherT[Future, SubmissionState, InboundMessageResponse](
          Future.successful(Right(GoodsReleasedResponse))
        )

      val getArrivalWithoutMessages: EitherT[Future, SubmissionState, ArrivalWithoutMessages] =
        EitherT[Future, SubmissionState, ArrivalWithoutMessages](
          Future.successful(Right(sampleArrival))
        )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalAndAudit(any(), any(), any())(any())).thenReturn(getArrivalWithoutMessages)
      when(mockMovementMessage.makeMovementMessage(any(), any(), any())).thenReturn(movementMessage)
      when(mockInboundMessageResponseService.makeInboundMessageResponse(any())).thenReturn(inboundMessageResponse)
      when(mockLockService.unlock(any())).thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[MovementMessageService].toInstance(mockMovementMessage))
        .overrides(bind[InboundMessageResponseService].toInstance(mockInboundMessageResponseService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        val expectedResult = InboundMessageRequest(sampleArrival, GoodsReleasedResponse, message)

        result.futureValue.value mustBe expectedResult
      }
    }

    "must return a DocumentExists error for an existing lock" in {

      val inboundXml = <CC044A></CC044A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      when(mockLockService.lock(any())).thenReturn(Future.successful(Left(DocumentExistsError("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe an[DocumentExistsError]
      }
    }

    "must return a submission state when there is a failure to retrieve a arrival" in {

      case class GetArrivalFailure(message: String) extends SubmissionState {
        override val monitorMessage: String = "monitorMessage"
      }

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)
      val sampleArrival = arbitrary[Arrival].sample.value

      val message = arbitrary[MovementMessageWithoutStatus].sample.value.copy(messageId = sampleArrival.nextMessageId)

      val movementMessage: EitherT[Future, SubmissionState, MovementMessageWithoutStatus] =
        EitherT[Future, SubmissionState, MovementMessageWithoutStatus](
          Future.successful(Right(message))
        )

      val inboundMessageResponse: EitherT[Future, SubmissionState, InboundMessageResponse] =
        EitherT[Future, SubmissionState, InboundMessageResponse](
          Future.successful(Right(GoodsReleasedResponse))
        )

      val getArrivalWithoutMessages: EitherT[Future, SubmissionState, ArrivalWithoutMessages] =
        EitherT[Future, SubmissionState, ArrivalWithoutMessages](
          Future.successful(Left(GetArrivalFailure("error")))
        )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockMovementMessage.makeMovementMessage(any(), any(), any())).thenReturn(movementMessage)
      when(mockInboundMessageResponseService.makeInboundMessageResponse(any())).thenReturn(inboundMessageResponse)
      when(mockGetArrivalService.getArrivalAndAudit(any(), any(), any())(any())).thenReturn(getArrivalWithoutMessages)

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[MovementMessageService].toInstance(mockMovementMessage))
        .overrides(bind[InboundMessageResponseService].toInstance(mockInboundMessageResponseService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe GetArrivalFailure("error")
      }
    }

    "must return a submission state when an inbound message response cannot be made" in {

      case class InboundMessageFailure(message: String) extends SubmissionState {
        override val monitorMessage: String = "monitorMessage"
      }

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val inboundMessageResponse: EitherT[Future, SubmissionState, InboundMessageResponse] =
        EitherT[Future, SubmissionState, InboundMessageResponse](
          Future.successful(Left(InboundMessageFailure("error")))
        )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockInboundMessageResponseService.makeInboundMessageResponse(any())).thenReturn(inboundMessageResponse)

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[InboundMessageResponseService].toInstance(mockInboundMessageResponseService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe InboundMessageFailure("error")
      }
    }

    "must return a submission state when a movement message cannot be made" in {

      case class MovementMessageFailure(message: String) extends SubmissionState {
        override val monitorMessage: String = "monitorMessage"
      }

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)
      val sampleArrival = arbitrary[ArrivalWithoutMessages].sample.value

      val inboundMessageResponse: EitherT[Future, SubmissionState, InboundMessageResponse] =
        EitherT[Future, SubmissionState, InboundMessageResponse](
          Future.successful(Right(GoodsReleasedResponse))
        )

      val movementMessage: EitherT[Future, SubmissionState, MovementMessageWithoutStatus] =
        EitherT[Future, SubmissionState, MovementMessageWithoutStatus](
          Future.successful(Left(MovementMessageFailure("error")))
        )

      val getArrivalWithoutMessages: EitherT[Future, SubmissionState, ArrivalWithoutMessages] =
        EitherT[Future, SubmissionState, ArrivalWithoutMessages](
          Future.successful(Right(sampleArrival))
        )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalAndAudit(any(), any(), any())(any())).thenReturn(getArrivalWithoutMessages)
      when(mockInboundMessageResponseService.makeInboundMessageResponse(any())).thenReturn(inboundMessageResponse)
      when(mockMovementMessage.makeMovementMessage(any(), any(), any())).thenReturn(movementMessage)

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[MovementMessageService].toInstance(mockMovementMessage))
        .overrides(bind[InboundMessageResponseService].toInstance(mockInboundMessageResponseService))
        .overrides(bind[GetArrivalService].toInstance(mockGetArrivalService))
        .build()

      running(application) {
        val service = application.injector.instanceOf[InboundRequestService]

        val result = service.makeInboundRequest(ArrivalId(0), inboundXml, messageSender)

        result.futureValue.left.value mustBe MovementMessageFailure("error")
      }
    }

    "must return a FailedToUnlock when unlocking fails" in {

      val inboundXml = <CC025A></CC025A>

      val messageSender = MessageSender(ArrivalId(0), 0)

      val sampleArrival = arbitrary[ArrivalWithoutMessages].sample.value

      val message = arbitrary[MovementMessageWithoutStatus].sample.value.copy(messageCorrelationId = sampleArrival.nextMessageCorrelationId)

      val movementMessage: EitherT[Future, SubmissionState, MovementMessageWithoutStatus] =
        EitherT[Future, SubmissionState, MovementMessageWithoutStatus](
          Future.successful(Right(message))
        )

      val inboundMessageResponse: EitherT[Future, SubmissionState, InboundMessageResponse] =
        EitherT[Future, SubmissionState, InboundMessageResponse](
          Future.successful(Right(GoodsReleasedResponse))
        )

      val getArrivalWithoutMessages: EitherT[Future, SubmissionState, ArrivalWithoutMessages] =
        EitherT[Future, SubmissionState, ArrivalWithoutMessages](
          Future.successful(Right(sampleArrival))
        )

      when(mockLockService.lock(any())).thenReturn(Future.successful(Right(())))
      when(mockGetArrivalService.getArrivalAndAudit(any(), any(), any())(any())).thenReturn(getArrivalWithoutMessages)
      when(mockMovementMessage.makeMovementMessage(any(), any(), any())).thenReturn(movementMessage)
      when(mockInboundMessageResponseService.makeInboundMessageResponse(any())).thenReturn(inboundMessageResponse)
      when(mockLockService.unlock(any())).thenReturn(Future.successful(Left(FailedToUnlock("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[LockService].toInstance(mockLockService))
        .overrides(bind[MovementMessageService].toInstance(mockMovementMessage))
        .overrides(bind[InboundMessageResponseService].toInstance(mockInboundMessageResponseService))
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
