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
import config.Constants
import generators.ModelGenerators
import models.ArrivalStatus.ArrivalSubmitted
import models.ArrivalStatus.GoodsReleased
import models.ChannelType.web
import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import models.Box
import models.BoxId
import models.DocumentExistsError
import models.FailedToSaveMessage
import models.FailedToValidateMessage
import models.GoodsReleasedResponse
import models.InboundMessageRequest
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
import services.InboundRequestService
import services.PushPullNotificationService
import services.SaveMessageService
import utils.Format

import java.time.LocalDateTime
import scala.concurrent.Future
import scala.xml.Utility.trim

class NCTSMessageControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  private val mockSaveMessageService: SaveMessageService                   = mock[SaveMessageService]
  private val mockPushPullNotificationService: PushPullNotificationService = mock[PushPullNotificationService]
  private val mockInboundRequestService: InboundRequestService             = mock[InboundRequestService]

  private val arrivalId     = ArrivalId(1)
  private val version       = 1
  private val messageSender = MessageSender(arrivalId, version)

  private val arrivalWithoutBox = Arbitrary
    .arbitrary[Arrival]
    .sample
    .value
    .copy(
      status = ArrivalSubmitted
    )

  private val testBoxId = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
  private val testBox   = Box(BoxId(testBoxId), Constants.BoxName)

  private val arrivalWithBox = arrivalWithoutBox.copy(notificationBox = Some(testBox))

  private val invalidXml = <test></test>

  private val dateTime = LocalDateTime.now

  private val xml =
    (<CC007A>
      <DatOfPreMES9>
        {Format.dateFormatted(dateTime)}
      </DatOfPreMES9>
      <TimOfPreMES10>
        {Format.timeFormatted(dateTime.toLocalTime)}
      </TimOfPreMES10>
    </CC007A>).map(trim)

  override def beforeEach: Unit = {
    super.beforeEach()
    reset(mockSaveMessageService)
    reset(mockPushPullNotificationService)
    reset(mockInboundRequestService)
  }

  "post" - {

    "must return OK, when the service validates and save the message" in {

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrivalWithoutBox, GoodsReleased, GoodsReleasedResponse, message))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(bind[SaveMessageService].toInstance(mockSaveMessageService))
        .overrides(bind[InboundRequestService].toInstance(mockInboundRequestService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
          .withXmlBody(xml)

        val result = route(application, request).value

        status(result) mustEqual OK
        header(LOCATION, result) mustBe Some(routes.MessagesController.getMessage(arrivalWithoutBox.arrivalId, arrivalWithoutBox.nextMessageId).url)
      }
    }

    "must return NotFound for an arrivalWithoutBox that does not exist" in {

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any())(any()))
        .thenReturn(Future.successful(Left(ArrivalNotFoundError("error"))))

      val application = baseApplicationBuilder.overrides(bind[InboundRequestService].toInstance(mockInboundRequestService)).build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withXmlBody(xml)
          .withHeaders("channel" -> web.toString)

        val result = route(application, request).value

        status(result) mustEqual NOT_FOUND
        verifyNoInteractions(mockSaveMessageService)
      }
    }

    "must return Internal Server Error if adding the message to the movement fails" in {

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrivalWithoutBox, GoodsReleased, GoodsReleasedResponse, message))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Left(FailedToSaveMessage("ERROR"))))

      val application = baseApplicationBuilder
        .overrides(bind[SaveMessageService].toInstance(mockSaveMessageService))
        .overrides(bind[InboundRequestService].toInstance(mockInboundRequestService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withXmlBody(xml)
          .withHeaders("channel" -> arrivalWithoutBox.channel.toString)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }

    "must return BadRequest error when failure to validate message" in {

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrivalWithoutBox, GoodsReleased, GoodsReleasedResponse, message))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Left(FailedToValidateMessage("error"))))

      val application = baseApplicationBuilder
        .overrides(
          bind[SaveMessageService].toInstance(mockSaveMessageService),
          bind[InboundRequestService].toInstance(mockInboundRequestService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
          .withXmlBody(xml)

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        verify(mockSaveMessageService, times(1)).saveInboundMessage(any(), any())(any())
      }
    }

    "must not send push notification when there is no notificationBox present" in {

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrivalWithoutBox, GoodsReleased, GoodsReleasedResponse, message))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[SaveMessageService].toInstance(mockSaveMessageService),
          bind[PushPullNotificationService].toInstance(mockPushPullNotificationService),
          bind[InboundRequestService].toInstance(mockInboundRequestService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
          .withXmlBody(xml)

        val result = route(application, request).value

        status(result) mustEqual OK
        verifyNoInteractions(mockPushPullNotificationService)
      }
    }

    "must send push notification when there is a notificationBox and valid timestamp present" in {
      def boxIdMatcher = refEq(testBoxId).asInstanceOf[BoxId]

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrivalWithBox, GoodsReleased, GoodsReleasedResponse, message))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Right(())))

      when(mockPushPullNotificationService.sendPushNotification(boxIdMatcher, any())(any(), any())).thenReturn(Future.unit)

      val application = baseApplicationBuilder
        .overrides(
          bind[SaveMessageService].toInstance(mockSaveMessageService),
          bind[PushPullNotificationService].toInstance(mockPushPullNotificationService),
          bind[InboundRequestService].toInstance(mockInboundRequestService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrivalWithBox.channel.toString)
          .withXmlBody(xml)

        val result = route(application, request).value

        status(result) mustEqual OK
        verify(mockPushPullNotificationService, times(1)).sendPushNotification(boxIdMatcher, any())(any(), any())
      }
    }

    "must not send push notification when timestamp cannot be parsed" in {
      def boxIdMatcher = refEq(testBoxId).asInstanceOf[BoxId]

      val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any(), any())(any()))
        .thenReturn(Future.successful(Right(InboundMessageRequest(arrivalWithBox, GoodsReleased, GoodsReleasedResponse, message))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Right(())))

      when(mockPushPullNotificationService.sendPushNotification(boxIdMatcher, any())(any(), any())).thenReturn(Future.unit)

      val application = baseApplicationBuilder
        .overrides(
          bind[SaveMessageService].toInstance(mockSaveMessageService),
          bind[PushPullNotificationService].toInstance(mockPushPullNotificationService),
          bind[InboundRequestService].toInstance(mockInboundRequestService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
          .withXmlBody(invalidXml)

        val result = route(application, request).value

        status(result) mustEqual OK
        verifyNoInteractions(mockPushPullNotificationService)
      }
    }

    "must return Locked" in {

      when(mockInboundRequestService.makeInboundRequest(any(), any(), any())(any()))
        .thenReturn(Future.successful(Left(DocumentExistsError("error"))))

      when(mockSaveMessageService.saveInboundMessage(any(), any())(any()))
        .thenReturn(Future.successful(Right(())))

      val application = baseApplicationBuilder
        .overrides(bind[SaveMessageService].toInstance(mockSaveMessageService))
        .overrides(bind[InboundRequestService].toInstance(mockInboundRequestService))
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
          .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
          .withXmlBody(xml)

        val result = route(application, request).value

        status(result) mustEqual LOCKED
      }
    }
  }
}
