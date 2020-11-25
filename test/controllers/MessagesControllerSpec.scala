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
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import models.SubmissionProcessingResult
import models.response.ResponseArrivalWithMessages
import models.response.ResponseMovementMessage
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.SubmitMessageService
import utils.Format

import scala.xml.Utility.trim
import scala.concurrent.Future

class MessagesControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  implicit val responseArrivalWrite = ResponseArrivalWithMessages.writes
  implicit val responseMessageWrite = ResponseMovementMessage.writes

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val mrn = arbitrary[MovementReferenceNumber].sample.value

  val messageId = MessageId.fromIndex(0)

  val arrivalId = arbitrary[ArrivalId].sample.value

  val requestXmlBody =
    <CC044A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <SynVerNumMES2>1</SynVerNumMES2>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC044A>.map(trim)

  val savedXmlMessage =
    <CC044A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <SynVerNumMES2>1</SynVerNumMES2>
      <MesSenMES3>{MessageSender(arrivalId, 2).toString}</MesSenMES3>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC044A>

  val movementMessage = MovementMessageWithStatus(
    localDateTime,
    MessageType.UnloadingRemarks,
    savedXmlMessage.map(trim),
    SubmissionPending,
    2
  )

  val arrivalWithOneMessage: Gen[Arrival] = for {
    arrival <- arbitrary[Arrival]
  } yield {
    arrival.copy(
      arrivalId = arrivalId,
      movementReferenceNumber = mrn,
      eoriNumber = "eori",
      status = ArrivalStatus.Initialized,
      messages = NonEmptyList.one(movementMessage),
      nextMessageCorrelationId = movementMessage.messageCorrelationId,
      created = localDateTime,
      updated = localDateTime
    )
  }

  val arrival = arrivalWithOneMessage.sample.value

  "MessagesController" - {

    "post" - {

      "must return Accepted, add the message to the movement, send the message upstream and set the message state to SubmissionSucceeded" in {

        val mockArrivalMovementRepository                     = mock[ArrivalMovementRepository]
        val mockLockRepository                                = mock[LockRepository]
        val mockSubmitMessageService                          = mock[SubmitMessageService]
        val captor: ArgumentCaptor[MovementMessageWithStatus] = ArgumentCaptor.forClass(classOf[MovementMessageWithStatus])
        val mockAuditService                                  = mock[AuditService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), eqTo(ArrivalStatus.UnloadingRemarksSubmitted))(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)
          val result  = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MessagesController.getMessage(arrival.arrivalId, MessageId.fromIndex(1)).url)
          verify(mockSubmitMessageService, times(1)).submitMessage(eqTo(arrival.arrivalId),
                                                                   eqTo(MessageId.fromIndex(1)),
                                                                   captor.capture(),
                                                                   eqTo(ArrivalStatus.UnloadingRemarksSubmitted))(any())

          val arrivalMessage: MovementMessageWithStatus = captor.getValue
          arrivalMessage mustEqual movementMessage

          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.UnloadingRemarksSubmitted), any(), eqTo(api))(any())
        }
      }

      "must return NotFound if there is no Arrival Movement for that ArrivalId" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.MessagesController.post(ArrivalId(1)).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual NOT_FOUND
        }
      }

      "must return InternalServerError if there was an internal failure when saving and sending" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must return BadRequest if the payload is missing SynVerNumMES2 node" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC044A>
            <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn.value}</DocNumHEA5>
            </HEAHEA>
          </CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC044A><SynVerNumMES2>1</SynVerNumMES2><HEAHEA></HEAHEA></CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message is not supported" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC045A><SynVerNumMES2>1</SynVerNumMES2><HEAHEA></HEAHEA></CC045A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadGateway if there was an external failure when saving and sending" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_GATEWAY
        }
      }

      "must return BadRequest if there has been a rejection from EIS dues to schema validation failing" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureRejected(ErrorInPayload.responseBody)))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return InternalServerError if there has been a rejection from EIS due to virus found or invalid token" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }

    "getMessage" - {
      "must return Ok with the retrieved message and state SubmissionSuccessful" in {

        val message = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

        when(mockArrivalMovementRepository.get(any()))
          .thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessage(arrival.arrivalId, MessageId.fromIndex(0)).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(ResponseMovementMessage.build(arrival.arrivalId, MessageId.fromIndex(0), message))
        }
      }

      "must return Ok with the retrieved message when it has no state" in {
        val message = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value
        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        when(mockArrivalMovementRepository.get(any()))
          .thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessage(arrival.arrivalId, MessageId.fromIndex(0)).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(ResponseMovementMessage.build(arrival.arrivalId, MessageId.fromIndex(0), message))
        }
      }

      "must return NOT FOUND" - {
        "when arrival is not found" in {
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(None))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessage(ArrivalId(1), MessageId.fromIndex(0)).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when message does not exist" in {
          val message = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessage(arrival.arrivalId, MessageId.fromIndex(5)).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when status is not equal to successful" in {
          val message = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionFailed)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessage(arrival.arrivalId, MessageId.fromIndex(0)).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when message belongs to an arrival the user cannot access" in {
          val message = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori2")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessage(arrival.arrivalId, MessageId.fromIndex(0)).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

      }
    }

    "getMessages" - {

      "must return OK" - {
        "with the retrieved messages" in {

          val message = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

          val expectedMessages = ResponseMovementMessage.build(arrival.arrivalId, MessageId.fromMessageIdValue(1).value, message)
          val expectedArrival  = ResponseArrivalWithMessages.build(arrival).copy(messages = Seq(expectedMessages))

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            lazy val request = FakeRequest(GET, routes.MessagesController.getMessages(arrival.arrivalId).url)
            val result       = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result) mustEqual Json.toJson(expectedArrival)
          }
        }

        "with only messages that are successful" in {
          val message1 = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
          val message2 = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionFailed)
          val arrival  = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.of(message1, message2), eoriNumber = "eori")

          val expectedMessages = ResponseMovementMessage.build(arrival.arrivalId, MessageId.fromMessageIdValue(1).value, message1)
          val expectedArrival  = ResponseArrivalWithMessages.build(arrival).copy(messages = Seq(expectedMessages))

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessages(arrival.arrivalId).url)
            val result  = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result) mustEqual Json.toJson(expectedArrival)
          }

        }

        "with only messages that are successful and stateless" in {
          val message1 = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
          val message2 = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionFailed)
          val message3 = Arbitrary.arbitrary[MovementMessageWithoutStatus].sample.value

          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.of(message1, message2, message3), eoriNumber = "eori")

          val expectedMessage1 = ResponseMovementMessage.build(arrival.arrivalId, MessageId.fromMessageIdValue(1).value, message1)
          val expectedMessage3 = ResponseMovementMessage.build(arrival.arrivalId, MessageId.fromMessageIdValue(3).value, message3)
          val expectedArrival  = ResponseArrivalWithMessages.build(arrival).copy(messages = Seq(expectedMessage1, expectedMessage3))

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessages(arrival.arrivalId).url)
            val result  = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result) mustEqual Json.toJson(expectedArrival)
          }
        }

        "with no messages if they are all failures" in {
          val message1 = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionFailed)
          val message2 = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionFailed)

          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.of(message1, message2), eoriNumber = "eori")

          val expectedArrival = ResponseArrivalWithMessages.build(arrival).copy(messages = Nil)

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessages(arrival.arrivalId).url)
            val result  = route(application, request).value

            status(result) mustEqual OK
            contentAsJson(result) mustEqual Json.toJson(expectedArrival)
          }
        }
      }

      "must return NOT FOUND" - {
        "when arrival is not found" in {
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(None))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessages(ArrivalId(1)).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when arrival is inaccessible to the user" in {
          val message = Arbitrary.arbitrary[MovementMessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = NonEmptyList.of(message), eoriNumber = "eori2")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MessagesController.getMessages(arrival.arrivalId).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }
      }

    }

  }
}
