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

package controllers

import base.SpecBase
import generators.ModelGenerators
import models.ArrivalStatus.GoodsReleased
import models.ChannelType.web
import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import models.ArrivalWithoutMessages
import models.DocumentExistsError
import models.FailedToLock
import models.GoodsReleasedResponse
import models.InboundMessageRequest
import models.InvalidArrivalRootNodeError
import models.MessageSender
import models.MovementMessageWithoutStatus
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.MovementMessageOrchestratorService

import scala.concurrent.Future
import utils.Format

class NCTSMessageControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  private val mockMovementMessageOrchestratorService: MovementMessageOrchestratorService = mock[MovementMessageOrchestratorService]

  private val messageSender = MessageSender(ArrivalId(1), 1)

  override def beforeEach: Unit = {
    super.beforeEach()
    reset(mockMovementMessageOrchestratorService)
  }

  "post" - {

    "must return OK" in {

      val arrival = Arbitrary.arbitrary[ArrivalWithoutMessages].sample.value

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockMovementMessageOrchestratorService.saveNCTSMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrival, GoodsReleased, GoodsReleasedResponse, message))))

      val application = baseApplicationBuilder
        .overrides(bind[MovementMessageOrchestratorService].toInstance(mockMovementMessageOrchestratorService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrival.channel.toString)
          .withXmlBody(<test></test>)

        val result = route(application, request).value

        status(result) mustEqual OK
        header(LOCATION, result) mustBe Some(routes.MessagesController.getMessage(arrival.arrivalId, arrival.nextMessageId).url)
      }
    }

    "must return NotFound when the MovementMessageOrchestratorService returns an ArrivalNotFoundError" in {

      when(mockMovementMessageOrchestratorService.saveNCTSMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(Left(ArrivalNotFoundError("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[MovementMessageOrchestratorService].toInstance(mockMovementMessageOrchestratorService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withXmlBody(<test></test>)
          .withHeaders("channel" -> web.toString)

        val result = route(application, request).value

        status(result) mustEqual NOT_FOUND
      }
    }

    "must return an Internal Server Error when the MovementMessageOrchestratorService returns a Submission state of type InternalError" in {

      when(mockMovementMessageOrchestratorService.saveNCTSMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(Left(FailedToLock("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[MovementMessageOrchestratorService].toInstance(mockMovementMessageOrchestratorService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withXmlBody(<test></test>)
          .withHeaders("channel" -> web.toString)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }

    "must return a Bad Request when the MovementMessageOrchestratorService returns a Submission state of type ExternalError" in {

      when(mockMovementMessageOrchestratorService.saveNCTSMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(Left(InvalidArrivalRootNodeError("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[MovementMessageOrchestratorService].toInstance(mockMovementMessageOrchestratorService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withXmlBody(<test></test>)
          .withHeaders("channel" -> web.toString)

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
      }
    }

    "must return Locked when the MovementMessageOrchestratorService returns a DocumentExistsError" in {

      when(mockMovementMessageOrchestratorService.saveNCTSMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(Left(DocumentExistsError("error"))))

      val application = baseApplicationBuilder
        .overrides(bind[MovementMessageOrchestratorService].toInstance(mockMovementMessageOrchestratorService))
        .build()

      running(application) {

        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withXmlBody(<test></test>)
          .withHeaders("channel" -> web.toString)

        val result = route(application, request).value

        status(result) mustEqual LOCKED
      }
    }
  }
}
