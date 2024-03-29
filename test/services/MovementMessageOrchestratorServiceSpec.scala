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
import generators.ModelGenerators
import models.ArrivalId
import models.ArrivalWithoutMessages
import models.GoodsReleasedResponse
import models.InboundMessageRequest
import models.MessageSender
import models.MovementMessageWithoutStatus
import models.SubmissionState
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.mvc.Headers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MovementMessageOrchestratorServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  val mockInboundRequestService       = mock[InboundRequestService]
  val mockSaveMessageService          = mock[SaveMessageService]
  val mockPushPullNotificationService = mock[PushPullNotificationService]

  "MovementMessageOrchestratorService" - {

    "saveNCTSMessage" - {

      "must return a InboundMessageRequest when successfully processing a request xml" in {

        val arrival       = arbitrary[ArrivalWithoutMessages].sample.value
        val message       = arbitrary[MovementMessageWithoutStatus].sample.value
        val messageSender = MessageSender(arrival.arrivalId, arrival.nextMessageCorrelationId)
        val xml           = <test>TestXml</test>

        val expectedResult = InboundMessageRequest(arrival, GoodsReleasedResponse, message)

        when(mockInboundRequestService.makeInboundRequest(any(), any(), any())(any())).thenReturn(Future.successful(Right(expectedResult)))
        when(mockSaveMessageService.saveInboundMessage(any(), any())(any())).thenReturn(Future.successful(Right(())))
        when(mockPushPullNotificationService.sendPushNotification(any(), any())(any())).thenReturn(Future.unit)

        val service = new MovementMessageOrchestratorService(
          mockInboundRequestService,
          mockSaveMessageService,
          mockPushPullNotificationService
        )

        val result = service.saveNCTSMessage(messageSender, xml, Headers(("key", "value")))

        result.futureValue.value mustBe expectedResult
      }

      "must return a SubmissionState when the InboundRequestService fails" in {

        sealed case class ExampleInboundRequestFailure(message: String) extends SubmissionState {
          override val monitorMessage: String = "exampleMessage"
        }

        val messageSender = MessageSender(ArrivalId(0), 0)
        val xml           = <test>TestXml</test>

        when(mockInboundRequestService.makeInboundRequest(any(), any(), any())(any()))
          .thenReturn(Future.successful(Left(ExampleInboundRequestFailure("message"))))

        val service = new MovementMessageOrchestratorService(
          mockInboundRequestService,
          mockSaveMessageService,
          mockPushPullNotificationService
        )

        val result = service.saveNCTSMessage(messageSender, xml, Headers(("key", "value")))

        result.futureValue.left.value mustBe ExampleInboundRequestFailure("message")

      }

      "must return a SubmissionState when the SaveMessageService fails" in {

        sealed case class ExampleSaveMessageFailure(message: String) extends SubmissionState {
          override val monitorMessage: String = "exampleMessage"
        }

        val arrival       = arbitrary[ArrivalWithoutMessages].sample.value
        val message       = arbitrary[MovementMessageWithoutStatus].sample.value
        val messageSender = MessageSender(arrival.arrivalId, arrival.nextMessageCorrelationId)
        val xml           = <test>TestXml</test>

        val inboundMessageRequest = InboundMessageRequest(arrival, GoodsReleasedResponse, message)

        when(mockInboundRequestService.makeInboundRequest(any(), any(), any())(any())).thenReturn(Future.successful(Right(inboundMessageRequest)))
        when(mockSaveMessageService.saveInboundMessage(any(), any())(any())).thenReturn(Future.successful(Left(ExampleSaveMessageFailure("message"))))

        val service = new MovementMessageOrchestratorService(
          mockInboundRequestService,
          mockSaveMessageService,
          mockPushPullNotificationService
        )

        val result = service.saveNCTSMessage(messageSender, xml, Headers(("key", "value")))

        result.futureValue.left.value mustBe ExampleSaveMessageFailure("message")
      }
    }
  }
}
