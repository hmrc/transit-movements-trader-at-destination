/*
 * Copyright 2022 HM Revenue & Customs
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

import audit.AuditService
import audit.AuditType
import base.SpecBase
import cats.data.Ior
import cats.data.NonEmptyList
import config.Constants
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import controllers.actions.AuthenticateActionProvider
import controllers.actions.FakeAuthenticateActionProvider
import generators.ModelGenerators
import models.ChannelType.api
import models.ChannelType.web
import models.MessageStatus.SubmissionPending
import models.response.ResponseArrival
import models.response.ResponseArrivals
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.ArrivalWithoutMessages
import models.Box
import models.BoxId
import models.EORINumber
import models.MessageId
import models.MessageSender
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import models.SubmissionProcessingResult
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalIdRepository
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.PushPullNotificationService
import services.SubmitMessageService
import utils.Format

import java.time._
import java.time.format.DateTimeFormatter
import scala.concurrent.Future
import scala.xml.NodeSeq
import scala.xml.Utility.trim

class MovementsControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)
  val stubClock     = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

  val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  val arrivalId = arbitrary[ArrivalId].sample.value
  val mrn       = arbitrary[MovementReferenceNumber].sample.value

  val requestXmlBody: NodeSeq =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <SynVerNumMES2>1</SynVerNumMES2>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>

  def savedXmlMessage(messageCorrelationId: Int) =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <SynVerNumMES2>1</SynVerNumMES2>
      <MesSenMES3>{MessageSender(arrivalId, messageCorrelationId).toString}</MesSenMES3>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>

  def movementMessage(messageCorrelationId: Int): MovementMessageWithStatus = MovementMessageWithStatus(
    MessageId(1),
    localDateTime,
    MessageType.ArrivalNotification,
    savedXmlMessage(messageCorrelationId).map(trim),
    SubmissionPending,
    1
  )

  val initializedArrival = Arrival(
    arrivalId = arrivalId,
    channel = api,
    movementReferenceNumber = mrn,
    eoriNumber = "eori",
    created = localDateTime,
    updated = localDateTime,
    lastUpdated = localDateTime,
    nextMessageCorrelationId = movementMessage(1).messageCorrelationId + 1,
    messages = NonEmptyList.one(movementMessage(1)),
    notificationBox = None
  )

  "MovementsController" - {

    "post" - {
      "must return Accepted, create movement, send the message upstream and set the state to Submitted when there is no notification box" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockAuditService         = mock[AuditService]
        val mockNotificationService  = mock[PushPullNotificationService]

        val expectedMessage: MovementMessageWithStatus = movementMessage(1).copy(messageCorrelationId = 1)
        val newArrival =
          initializedArrival.copy(messages = NonEmptyList.of[MovementMessageWithStatus](expectedMessage), channel = web)
        val captor: ArgumentCaptor[Arrival] = ArgumentCaptor.forClass(classOf[Arrival])

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(newArrival.arrivalId))
        when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
            bind[AuditService].toInstance(mockAuditService),
            bind[Clock].toInstance(stubClock)
          )
          .build()

        running(application) {

          val request =
            FakeRequest(POST, routes.MovementsController.post.url)
              .withHeaders("channel" -> newArrival.channel.toString)
              .withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MovementsController.getArrival(newArrival.arrivalId).url)
          contentAsJson(result).as[Option[Box]] must be(None)

          verify(mockSubmitMessageService, times(1)).submitArrival(captor.capture())(any())

          val arrivalMessage: MovementMessageWithStatus = captor.getValue.messages.head.asInstanceOf[MovementMessageWithStatus]
          arrivalMessage.message.map(trim) mustEqual expectedMessage.message.map(trim)

          verify(mockSubmitMessageService, times(1)).submitArrival(eqTo(newArrival))(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.ArrivalNotificationSubmitted),
                                                        eqTo(Ior.right(EORINumber(newArrival.eoriNumber))),
                                                        any(),
                                                        any())(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.MesSenMES3Added), eqTo(Ior.right(EORINumber(newArrival.eoriNumber))), any(), any())(
            any())
          verifyNoInteractions(mockNotificationService)
        }
      }

      "must return Accepted, create movement, send the message upstream and set the state to Submitted when there is a notification box" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockAuditService         = mock[AuditService]
        val mockNotificationService  = mock[PushPullNotificationService]

        val testClientId = "X5ZasuQLH0xqKooV_IEw6yjQNfEa"
        val testBoxId    = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
        val testBox      = Box(BoxId(testBoxId), Constants.BoxName)

        val expectedMessage: MovementMessageWithStatus = movementMessage(1).copy(messageCorrelationId = 1)
        val newArrival =
          initializedArrival.copy(messages = NonEmptyList.of[MovementMessageWithStatus](expectedMessage), channel = web, notificationBox = Some(testBox))
        val captor: ArgumentCaptor[Arrival] = ArgumentCaptor.forClass(classOf[Arrival])

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(newArrival.arrivalId))
        when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(Some(testBox)))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
            bind[AuditService].toInstance(mockAuditService),
            bind[Clock].toInstance(stubClock)
          )
          .build()

        running(application) {

          val request =
            FakeRequest(POST, routes.MovementsController.post.url)
              .withHeaders("channel" -> newArrival.channel.toString, Constants.XClientIdHeader -> testClientId)
              .withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MovementsController.getArrival(newArrival.arrivalId).url)
          contentAsJson(result).as[Option[Box]] must be(Some(testBox))

          verify(mockSubmitMessageService, times(1)).submitArrival(captor.capture())(any())

          val arrivalMessage: MovementMessageWithStatus = captor.getValue.messages.head.asInstanceOf[MovementMessageWithStatus]
          arrivalMessage.message.map(trim) mustEqual expectedMessage.message.map(trim)

          verify(mockSubmitMessageService, times(1)).submitArrival(eqTo(newArrival))(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.ArrivalNotificationSubmitted),
                                                        eqTo(Ior.right(EORINumber(newArrival.eoriNumber))),
                                                        any(),
                                                        any())(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.MesSenMES3Added), eqTo(Ior.right(EORINumber(newArrival.eoriNumber))), any(), any())(
            any())
        }
      }

      "must return InternalServerError if the InternalReference generation fails" in {
        val mockArrivalIdRepository = mock[ArrivalIdRepository]
        val mockNotificationService = mock[PushPullNotificationService]

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.failed(new Exception))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be defined
        }
      }

      "must return InternalServerError if there was an internal failure when saving and sending" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockNotificationService  = mock[PushPullNotificationService]

        val arrivalId = ArrivalId(1)

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be defined
        }
      }

      "must return BadGateway if there was an external failure when saving and sending" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockNotificationService  = mock[PushPullNotificationService]

        val arrivalId = ArrivalId(1)

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_GATEWAY
        }
      }

      "must return BadRequest if the payload is missing SynVerNumMES2 node" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockNotificationService  = mock[PushPullNotificationService]

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[PushPullNotificationService].toInstance(mockNotificationService),
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
            )
            .build()

        running(application) {
          val requestXmlBody =
            <CC007A>
                <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
                <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
                <HEAHEA>
                  <DocNumHEA5>{mrn.value}</DocNumHEA5>
                </HEAHEA>
              </CC007A>

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be defined
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockNotificationService  = mock[PushPullNotificationService]

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[PushPullNotificationService].toInstance(mockNotificationService),
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
            )
            .build()

        running(application) {
          val requestXmlBody =
            <CC007A><SynVerNumMES2>1</SynVerNumMES2><HEAHEA><DocNumHEA5>MRN</DocNumHEA5></HEAHEA></CC007A>

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be defined
        }
      }

      "must return BadRequest if the message is not an arrival notification" in {
        val mockArrivalIdRepository = mock[ArrivalIdRepository]
        val mockNotificationService = mock[PushPullNotificationService]

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[PushPullNotificationService].toInstance(mockNotificationService),
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
            )
            .build()

        running(application) {
          val requestXmlBody = <InvalidRootNode><SynVerNumMES2>1</SynVerNumMES2></InvalidRootNode>

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be defined
        }
      }

      "must return BadRequest if the message has been rejected from EIS due to error in payload" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockNotificationService  = mock[PushPullNotificationService]

        val arrivalId = ArrivalId(1)

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockSubmitMessageService.submitArrival(any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureRejected(ErrorInPayload.responseBody)))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return InternalServerError if there has been a rejection from EIS due to virus found or invalid token" in {
        val mockArrivalIdRepository  = mock[ArrivalIdRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]
        val mockNotificationService  = mock[PushPullNotificationService]

        val arrivalId = ArrivalId(1)

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockSubmitMessageService.submitArrival(any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))
        when(mockNotificationService.getBox(any())(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[PushPullNotificationService].toInstance(mockNotificationService),
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.MovementsController.post.url).withHeaders("channel" -> web.toString).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }

    "putArrival" - {
      "must return Accepted, when the result of submission is a Success" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]
        val mockAuditService              = mock[AuditService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        val captor = ArgumentCaptor.forClass(classOf[MovementMessageWithStatus])

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        val expectedMessage: MovementMessageWithStatus = movementMessage(2).copy(messageCorrelationId = 2)

        running(application) {

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url)
            .withHeaders("channel" -> initializedArrival.channel.toString)
            .withXmlBody(requestXmlBody.map(trim))
          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MovementsController.getArrival(initializedArrival.arrivalId).url)
          verify(mockSubmitMessageService, times(1)).submitIe007Message(
            eqTo(initializedArrival.arrivalId),
            eqTo(MessageId(2)),
            captor.capture(),
            eqTo(initializedArrival.movementReferenceNumber),
            any()
          )(any())

          val movement: MovementMessageWithStatus = captor.getValue
          movement.messageCorrelationId mustEqual expectedMessage.messageCorrelationId
          movement.status mustEqual expectedMessage.status
          movement.messageType mustEqual expectedMessage.messageType
          movement.dateTime mustEqual expectedMessage.dateTime
          movement.message.map(trim) mustEqual expectedMessage.message.map(trim)

          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.ArrivalNotificationReSubmitted),
                                                        eqTo(Ior.right(EORINumber(initializedArrival.eoriNumber))),
                                                        any(),
                                                        any())(
            any()
          )
        }

      }

      "must return NotFound if there is no Arrival Movement for that ArrivalId" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector)
          )
          .build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC007A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <SynVerNumMES2>1</SynVerNumMES2>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(ArrivalId(1)).url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual NOT_FOUND

        }
      }

      "must return InternalServerError if there was an internal failure when saving and sending" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC007A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <SynVerNumMES2>1</SynVerNumMES2>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url)
            .withHeaders("channel" -> initializedArrival.channel.toString)
            .withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR

        }
      }

      "must return BadGateway if there was an external failure when saving and sending" in {

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

        val app = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(app) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC007A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <SynVerNumMES2>1</SynVerNumMES2>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url)
            .withHeaders("channel" -> initializedArrival.channel.toString)
            .withXmlBody(requestXmlBody.map(trim))

          val result = route(app, request).value

          status(result) mustEqual BAD_GATEWAY
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC007A><SynVerNumMES2>1</SynVerNumMES2><HEAHEA></HEAHEA></CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url)
            .withHeaders("channel" -> arrival.channel.toString)
            .withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message is not an arrival notification" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector)
            )
            .build()

        running(application) {
          val requestXmlBody = <InvalidRootNode><SynVerNumMES2>1</SynVerNumMES2></InvalidRootNode>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message has been rejected from EIS due to error in payload" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureRejected(ErrorInPayload.responseBody)))

        val application = baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC007A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <SynVerNumMES2>1</SynVerNumMES2>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST

        }
      }

      "must return InternalServerError if the message has been rejected from EIS due to virus found or invalid token" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC007A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <SynVerNumMES2>1</SynVerNumMES2>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url)
            .withXmlBody(requestXmlBody.map(trim))
            .withHeaders("channel" -> initializedArrival.channel.toString)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR

        }
      }
    }

    "getArrivals" - {

      "must return Ok with the retrieved arrivals" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {
          forAll(listWithMaxLength[ResponseArrival](10)) {
            arrivals =>
              val responseArrivals = ResponseArrivals(arrivals, arrivals.length, arrivals.length, arrivals.length)
              when(mockArrivalMovementRepository.fetchAllArrivals(any(), any(), any(), any(), any(), any())).thenReturn(Future.successful(responseArrivals))

              val request = FakeRequest(GET, routes.MovementsController.getArrivals().url)

              val result = route(application, request).value

              status(result) mustEqual OK
              contentAsJson(result) mustEqual Json.toJson(responseArrivals)

              reset(mockArrivalMovementRepository)
          }
        }
      }

      "must return Ok with the retrieved an arrival passing the Json into the correct format" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {

          val createdAndUpdatedDate = LocalDateTime.parse("2021-02-10T09:46:25.55")

          val arrivals = Seq(
            ResponseArrival(
              ArrivalId(12),
              "/some/location",
              "/messages/location",
              MovementReferenceNumber("1234567890"),
              ArrivalStatus.ArrivalRejected,
              createdAndUpdatedDate,
              createdAndUpdatedDate
            )
          )
          val responseArrivals = ResponseArrivals(arrivals, 1, 1, 1)
          when(mockArrivalMovementRepository.fetchAllArrivals(any(), any(), any(), any(), any(), any())).thenReturn(Future.successful(responseArrivals))

          val request = FakeRequest(GET, routes.MovementsController.getArrivals().url)

          val result = route(application, request).value

          status(result) mustEqual OK

          val expectedJson =
            s"""{
               |  "arrivals": [
               |    {
               |      "arrivalId": 12,
               |      "location": "/some/location",
               |      "messagesLocation": "/messages/location",
               |      "movementReferenceNumber": "1234567890",
               |      "status": "ArrivalRejected",
               |      "created": "${dateFormat.format(createdAndUpdatedDate)}",
               |      "updated": "${dateFormat.format(createdAndUpdatedDate)}"
               |    }
               |  ],
               |  "retrievedArrivals": 1,
               |  "totalArrivals": 1,
               |  "totalMatched": 1
               |}""".stripMargin

          contentAsJson(result) mustBe Json.parse(expectedJson)

          reset(mockArrivalMovementRepository)
        }
      }

      "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the Arrival Movements" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        when(mockArrivalMovementRepository.fetchAllArrivals(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.failed(new Exception))

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {

          val request = FakeRequest(GET, routes.MovementsController.getArrivals().url)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }

    "getArrival" - {

      "must return Ok with the retrieved arrival" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")
        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())).thenReturn(Future.successful(Some(ArrivalWithoutMessages.fromArrival(arrival))))

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url).withHeaders("channel" -> arrival.channel.toString)

          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(ResponseArrival.build(arrival))
        }
      }

      "must return Not Found if arrival doesn't exist" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())).thenReturn(Future.successful(None))

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url).withHeaders("channel" -> web.toString)
          val result  = route(application, request).value

          status(result) mustEqual NOT_FOUND
        }
      }

      "must return Not Found if arrival eori doesn't match" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori2")
        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())).thenReturn(Future.successful(Some(ArrivalWithoutMessages.fromArrival(arrival))))

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url).withHeaders("channel" -> arrival.channel.toString)
          val result  = route(application, request).value

          status(result) mustEqual NOT_FOUND
        }
      }

      "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the arrival movement" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        when(mockArrivalMovementRepository.getWithoutMessages(any(), any()))
          .thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url).withHeaders("channel" -> web.toString)
          val result  = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }

  }
}
