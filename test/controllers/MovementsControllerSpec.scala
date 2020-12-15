/*
 * Copyright 2020 HM Revenue & Customs
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

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import audit.AuditService
import audit.AuditType
import base.SpecBase
import cats.data.NonEmptyList
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import controllers.actions.AuthenticatedGetOptionalArrivalForWriteActionProvider
import controllers.actions.FakeAuthenticatedGetOptionalArrivalForWriteActionProvider
import generators.ModelGenerators
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.ChannelType.api
import models.MessageId
import models.MessageSender
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import models.ResponseArrivals
import models.SubmissionProcessingResult
import models.response.ResponseArrival
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalIdRepository
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.SubmitMessageService
import utils.Format

import scala.concurrent.Future
import scala.xml.Utility.trim
import scala.xml.NodeSeq

class MovementsControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

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
    status = ArrivalStatus.Initialized,
    created = localDateTime,
    updated = localDateTime,
    lastUpdated = localDateTime,
    nextMessageCorrelationId = movementMessage(1).messageCorrelationId + 1,
    messages = NonEmptyList.one(movementMessage(1))
  )

  "MovementsController" - {

    "post" - {
      "when there are no previous failed attempts to submit" - {
        "must return Accepted, create movement, send the message upstream and set the state to Submitted" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]
          val mockAuditService         = mock[AuditService]

          val expectedMessage: MovementMessageWithStatus = movementMessage(1).copy(messageCorrelationId = 1)
          val newArrival                                 = initializedArrival.copy(messages = NonEmptyList.of[MovementMessageWithStatus](expectedMessage))
          val captor: ArgumentCaptor[Arrival]            = ArgumentCaptor.forClass(classOf[Arrival])

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(newArrival.arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider()),
              bind[AuditService].toInstance(mockAuditService)
            )
            .build()

          running(application) {

            val request =
              FakeRequest(POST, routes.MovementsController.post().url).withHeaders(("channel" -> api.toString)).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual ACCEPTED
            header("Location", result).value must be(routes.MovementsController.getArrival(newArrival.arrivalId).url)

            verify(mockSubmitMessageService, times(1)).submitArrival(captor.capture())(any())

            val arrivalMessage: MovementMessageWithStatus = captor.getValue.messages.head.asInstanceOf[MovementMessageWithStatus]
            arrivalMessage.message.map(trim) mustEqual expectedMessage.message.map(trim)

            verify(mockSubmitMessageService, times(1)).submitArrival(eqTo(newArrival))(any())
            verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.ArrivalNotificationSubmitted), any(), any())(any())
            verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.MesSenMES3Added), any(), any())(any())
          }
        }

        "must return InternalServerError if the InternalReference generation fails" in {
          val mockArrivalIdRepository = mock[ArrivalIdRepository]

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.failed(new Exception))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
            header("Location", result) must not be defined
          }
        }

        "must return InternalServerError if there was an internal failure when saving and sending" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          val arrivalId = ArrivalId(1)

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
            header("Location", result) must not be defined
          }
        }

        "must return BadGateway if there was an external failure when saving and sending" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          val arrivalId = ArrivalId(1)

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_GATEWAY
          }
        }

        "must return BadRequest if the payload is missing SynVerNumMES2 node" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          val application =
            baseApplicationBuilder
              .overrides(
                bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
                bind[SubmitMessageService].toInstance(mockSubmitMessageService),
                bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
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

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
            header("Location", result) must not be (defined)
          }
        }

        "must return BadRequest if the payload is malformed" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          val application =
            baseApplicationBuilder
              .overrides(
                bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
                bind[SubmitMessageService].toInstance(mockSubmitMessageService),
                bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
              )
              .build()

          running(application) {
            val requestXmlBody =
              <CC007A><SynVerNumMES2>1</SynVerNumMES2><HEAHEA><DocNumHEA5>MRN</DocNumHEA5></HEAHEA></CC007A>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
            header("Location", result) must not be (defined)
          }
        }

        "must return BadRequest if the message is not an arrival notification" in {
          val mockArrivalIdRepository = mock[ArrivalIdRepository]

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))

          val application =
            baseApplicationBuilder
              .overrides(
                bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
                bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
              )
              .build()

          running(application) {
            val requestXmlBody = <InvalidRootNode><SynVerNumMES2>1</SynVerNumMES2></InvalidRootNode>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
            header("Location", result) must not be (defined)
          }
        }

        "must return BadRequest if the message has been rejected from EIS due to error in payload" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          val arrivalId = ArrivalId(1)

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureRejected(ErrorInPayload.responseBody)))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
          }
        }

        "must return InternalServerError if there has been a rejection from EIS due to virus found or invalid token" in {
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          val arrivalId = ArrivalId(1)

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider())
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
          }
        }
      }

      "when there has been a previous failed attempt to submit" - {

        val failedToSubmit007     = movementMessage(1).copy(status = SubmissionFailed)
        val failedToSubmitArrival = initializedArrival.copy(messages = NonEmptyList.one(failedToSubmit007))

        "must return Accepted when submitted to upstream  against the existing arrival" in {

          val mockSubmitMessageService = mock[SubmitMessageService]
          val captor                   = ArgumentCaptor.forClass(classOf[MovementMessageWithStatus])
          val mockAuditService         = mock[AuditService]

          when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

          val application = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(
                FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(failedToSubmitArrival)),
              bind[AuditService].toInstance(mockAuditService)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result                                     = route(application, request).value
            val expectedMessage: MovementMessageWithStatus = movementMessage(2).copy(messageCorrelationId = 2)

            status(result) mustEqual ACCEPTED
            header("Location", result).value must be(routes.MovementsController.getArrival(initializedArrival.arrivalId).url)
            verify(mockSubmitMessageService, times(1)).submitMessage(eqTo(initializedArrival.arrivalId),
                                                                     eqTo(MessageId.fromIndex(1)),
                                                                     captor.capture(),
                                                                     eqTo(ArrivalStatus.ArrivalSubmitted))(any())

            val movement: MovementMessageWithStatus = captor.getValue
            movement.messageCorrelationId mustEqual expectedMessage.messageCorrelationId
            movement.status mustEqual expectedMessage.status
            movement.messageType mustEqual expectedMessage.messageType
            movement.dateTime mustEqual expectedMessage.dateTime
            movement.message.map(trim) mustEqual expectedMessage.message.map(trim)

            verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.ArrivalNotificationSubmitted), any(), any())(any())
            verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.MesSenMES3Added), any(), any())(any())

          }
        }

        "must return Accepted when and saved as a new arrival movement when there has been a successful message" in {
          val captor: ArgumentCaptor[Arrival] = ArgumentCaptor.forClass(classOf[Arrival])
          val messages = NonEmptyList.of(
            movementMessage(1).copy(status = SubmissionPending, messageCorrelationId = 1),
            movementMessage(2).copy(status = SubmissionFailed, messageCorrelationId = 2),
            movementMessage(3).copy(status = SubmissionSucceeded, messageCorrelationId = 3)
          )
          val arrival = initializedArrival.copy(messages = messages, nextMessageCorrelationId = 4)

          val expectedArrival = initializedArrival.copy(messages = NonEmptyList.of(movementMessage(1)))

          val mockSubmitMessageService = mock[SubmitMessageService]
          val mockArrivalIdRepository  = mock[ArrivalIdRepository]

          when(mockSubmitMessageService.submitArrival(any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(expectedArrival.arrivalId))

          val application = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(arrival))
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual ACCEPTED
            header("Location", result).value must be(routes.MovementsController.getArrival(expectedArrival.arrivalId).url)
            verify(mockSubmitMessageService, times(1)).submitArrival(captor.capture())(any())

            val arrivalMessage: MovementMessageWithStatus = captor.getValue.messages.head.asInstanceOf[MovementMessageWithStatus]
            arrivalMessage.message.map(trim) mustEqual movementMessage(1).message.map(trim)
          }
        }

        "must return InternalServerError if there was an internal failure when saving and sending" in {
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(
                FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(initializedArrival))
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR

          }
        }

        "must return BadGateway if there was an external failure when saving and sending" in {
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

          val app = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(
                FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(initializedArrival))
            )
            .build()

          running(app) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(app, request).value

            status(result) mustEqual BAD_GATEWAY
          }
        }

        "must return BadRequest if the payload is malformed" in {
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")

          val application =
            baseApplicationBuilder
              .overrides(
                bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(arrival))
              )
              .build()

          running(application) {
            val requestXmlBody = <CC007A><SynVerNumMES2>1</SynVerNumMES2><HEAHEA></HEAHEA></CC007A>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
          }
        }

        "must return BadRequest if the message is not an arrival notification" in {

          val application =
            baseApplicationBuilder
              .overrides(
                bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(
                  FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(initializedArrival))
              )
              .build()

          running(application) {

            val requestXmlBody = <InvalidRootNode><SynVerNumMES2>1</SynVerNumMES2></InvalidRootNode>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
          }
        }

        "must return BadRequest if the message has been rejected from EIS due to error in payload" in {
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureRejected(ErrorInPayload.responseBody)))

          val app = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(
                FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(initializedArrival))
            )
            .build()

          running(app) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(app, request).value

            status(result) mustEqual BAD_REQUEST
          }
        }

        "must return InternalServerError if there has been a rejection from EIS due to virus found or invalid token" in {
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
            .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

          val app = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[AuthenticatedGetOptionalArrivalForWriteActionProvider].toInstance(
                FakeAuthenticatedGetOptionalArrivalForWriteActionProvider(initializedArrival))
            )
            .build()

          running(app) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody.map(trim))

            val result = route(app, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
          }
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
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any())(any()))
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

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))
          val result  = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MovementsController.getArrival(initializedArrival.arrivalId).url)
          verify(mockSubmitMessageService, times(1)).submitIe007Message(eqTo(initializedArrival.arrivalId),
                                                                        eqTo(MessageId.fromIndex(1)),
                                                                        captor.capture(),
                                                                        eqTo(initializedArrival.movementReferenceNumber))(any())

          val movement: MovementMessageWithStatus = captor.getValue
          movement.messageCorrelationId mustEqual expectedMessage.messageCorrelationId
          movement.status mustEqual expectedMessage.status
          movement.messageType mustEqual expectedMessage.messageType
          movement.dateTime mustEqual expectedMessage.dateTime
          movement.message.map(trim) mustEqual expectedMessage.message.map(trim)

          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.ArrivalNotificationReSubmitted), any(), any())(any())
        }

      }

      "must return NotFound if there is no Arrival Movement for that ArrivalId" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
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

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(ArrivalId(1)).url).withXmlBody(requestXmlBody.map(trim))

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
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any())(any()))
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

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))

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
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any())(any()))
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

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))

          val result = route(app, request).value

          status(result) mustEqual BAD_GATEWAY
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
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

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message is not an arrival notification" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
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
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any())(any()))
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
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(initializedArrival)))

        when(mockSubmitMessageService.submitIe007Message(any(), any(), any(), any())(any()))
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

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(initializedArrival.arrivalId).url).withXmlBody(requestXmlBody.map(trim))

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
          forAll(listWithMaxLength[Arrival](10)) {
            arrivals =>
              when(mockArrivalMovementRepository.fetchAllArrivals(any(), any())).thenReturn(Future.successful(arrivals))

              val request = FakeRequest(GET, routes.MovementsController.getArrivals().url)

              val result = route(application, request).value

              status(result) mustEqual OK
              contentAsJson(result) mustEqual Json.toJson(ResponseArrivals(arrivals.map {
                a =>
                  ResponseArrival.build(a)
              }))

              reset(mockArrivalMovementRepository)
          }
        }
      }

      "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the Arrival Movements" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        when(mockArrivalMovementRepository.fetchAllArrivals(any(), any()))
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
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url)

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

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url)
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
        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url)
          val result  = route(application, request).value

          status(result) mustEqual NOT_FOUND
        }
      }

      "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the arrival movement" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        when(mockArrivalMovementRepository.get(any(), any()))
          .thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
          .build()

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getArrival(ArrivalId(1)).url)
          val result  = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }

  }
}
