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
import controllers.actions.FakeInboundMessageBadRequestTransformer
import controllers.actions.FakeMessageTransformer
import controllers.actions.MessageTransformerInterface
import generators.ModelGenerators
import models.ChannelType.web
import models.Arrival
import models.ArrivalId
import models.Box
import models.BoxId
import models.MessageSender
import models.SubmissionProcessingResult
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.PushPullNotificationService
import services.SaveMessageService

import scala.concurrent.Future
import java.time.LocalDateTime
import utils.Format

class NCTSMessageControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  private val mockArrivalMovementRepository: ArrivalMovementRepository     = mock[ArrivalMovementRepository]
  private val mockLockRepository: LockRepository                           = mock[LockRepository]
  private val mockSaveMessageService: SaveMessageService                   = mock[SaveMessageService]
  private val mockPushPullNotificationService: PushPullNotificationService = mock[PushPullNotificationService]

  private val arrivalId     = ArrivalId(1)
  private val version       = 1
  private val messageSender = MessageSender(arrivalId, version)

  private val arrivalWithoutBox = Arbitrary.arbitrary[Arrival].sample.value

  private val testBoxId = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
  private val testBox   = Box(BoxId(testBoxId), Constants.BoxName)

  private val arrivalWithBox = arrivalWithoutBox.copy(notificationBox = Some(testBox))

  private val invalidXml = <test></test>

  private val dateTime = LocalDateTime.now

  private val xml =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(dateTime)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(dateTime.toLocalTime)}</TimOfPreMES10>
    </CC007A>

  override def beforeEach: Unit = {
    super.beforeEach()
    reset(mockArrivalMovementRepository)
    reset(mockLockRepository)
    reset(mockSaveMessageService)
    reset(mockPushPullNotificationService)
  }

  "post" - {

    "when a lock can be acquired" - {
      "must return OK, when the service validates and save the message" in {

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithoutBox)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[MessageTransformerInterface].to[FakeMessageTransformer]
          )
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

      "must lock, return oK and unlock when given a message for an arrivalWithoutBox that does not exist" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(xml)
            .withHeaders("channel" -> web.toString)

          val result = route(application, request).value

          status(result) mustEqual OK
          verify(mockArrivalMovementRepository, never).addResponseMessage(any(), any(), any())
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return Internal Server Error and unlock if adding the message to the movement fails" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithoutBox)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[MessageTransformerInterface].to[FakeMessageTransformer]
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(xml)
            .withHeaders("channel" -> arrivalWithoutBox.channel.toString)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock the arrivalWithoutBox, return BadRequest error and unlock when an XMessageType is invalid" in {

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithoutBox)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[MessageTransformerInterface].to[FakeInboundMessageBadRequestTransformer]
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(xml)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockSaveMessageService, never()).validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any())
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock the arrivalWithoutBox, return BadRequest error and unlock when fail to validate message" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithoutBox)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[MessageTransformerInterface].to[FakeMessageTransformer]
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
            .withXmlBody(xml)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockSaveMessageService, times(1)).validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any())
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must not send push notification when there is no notificationBox present" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithoutBox)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[PushPullNotificationService].toInstance(mockPushPullNotificationService),
            bind[MessageTransformerInterface].to[FakeMessageTransformer]
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

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithBox)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockPushPullNotificationService.sendPushNotification(boxIdMatcher, any())(any(), any())).thenReturn(Future.unit)

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[PushPullNotificationService].toInstance(mockPushPullNotificationService),
            bind[MessageTransformerInterface].to[FakeMessageTransformer]
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withHeaders("channel" -> arrivalWithoutBox.channel.toString)
            .withXmlBody(xml)

          val result = route(application, request).value

          status(result) mustEqual OK
          verify(mockPushPullNotificationService, times(1)).sendPushNotification(boxIdMatcher, any())(any(), any())
        }
      }

      "must not send push notification when timestamp cannot be parsed" in {
        def boxIdMatcher = refEq(testBoxId).asInstanceOf[BoxId]

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrivalWithBox)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockPushPullNotificationService.sendPushNotification(boxIdMatcher, any())(any(), any())).thenReturn(Future.unit)

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[PushPullNotificationService].toInstance(mockPushPullNotificationService),
            bind[MessageTransformerInterface].to[FakeMessageTransformer]
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

    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrivalWithoutBox)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(false))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(xml)

          val result = route(application, request).value

          status(result) mustEqual LOCKED
        }
      }
    }
  }
}
