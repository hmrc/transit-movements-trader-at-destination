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

import base.SpecBase
import connectors.MessageConnector
import generators.ModelGenerators
import models.ArrivalState.ArrivalSubmitted
import models.MessageState.SubmissionFailed
import models.MessageState.SubmissionPending
import models.MessageState.SubmissionSucceeded
import models.Arrival
import models.ArrivalId
import models.ArrivalState
import models.Arrivals
import models.MessageId
import models.MessageState
import models.MovementMessage
import models.MovementMessageWithState
import models.MovementMessageWithoutState
import models.response.ResponseMovementMessage
import models.MessageType
import models.MovementMessageWithState
import models.MovementReferenceNumber
import models.SubmissionResult
import org.mockito.Matchers.any
import org.mockito.Matchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
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
import services.SubmitMessageService
import uk.gov.hmrc.http.HttpException
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.Upstream5xxResponse
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class MovementsControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val arrivalId = arbitrary[ArrivalId].sample.value
  val mrn       = arbitrary[MovementReferenceNumber].sample.value

  val requestXmlBody =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>

  val messageId = new MessageId(0)

  val movementMessge = MovementMessageWithState(
    localDateTime,
    MessageType.ArrivalNotification,
    requestXmlBody,
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
      state = ArrivalState.Initialized,
      messages = Seq(movementMessge),
      nextMessageCorrelationId = movementMessge.messageCorrelationId,
      created = localDateTime,
      updated = localDateTime
    )
  }

  val arrival = arrivalWithOneMessage.sample.value

  "MovementsController" - {

    "createMovement" - {
      "when there are no previous failed attempts to submit" - {
        "must return Accepted, create movement, send the message upstream and set the state to Submitted" in {
          val mockArrivalIdRepository       = mock[ArrivalIdRepository]
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          val mockSubmitMessageService      = mock[SubmitMessageService]

          val expectedMessage = movementMessge.copy(messageCorrelationId = 1)
          val newArrival      = arrival.copy(messages = Seq(expectedMessage))

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(newArrival.arrivalId))
          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))
          when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionResult.Success))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual ACCEPTED
            header("Location", result).value must be(routes.MovementsController.getArrival(newArrival.arrivalId).url)
            verify(mockSubmitMessageService, times(1)).submitArrival(eqTo(newArrival))(any())
          }
        }

        "must return InternalServerError if the InternalReference generation fails" in {
          val mockArrivalIdRepository       = mock[ArrivalIdRepository]
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          val mockMessageConnector          = mock[MessageConnector]

          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.failed(new Exception))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
            header("Location", result) must not be defined
            verify(mockArrivalMovementRepository, never()).insert(any())
          }
        }

        "must return InternalServerError if there was an internal failure when saving and sending" in {
          val mockArrivalIdRepository       = mock[ArrivalIdRepository]
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          val mockSubmitMessageService      = mock[SubmitMessageService]

          val arrivalId = ArrivalId(1)

          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionResult.FailureInternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
            header("Location", result) must not be defined
          }
        }

        "must return BadGateway if there was an external failure when saving and sending" in {
          val mockArrivalIdRepository       = mock[ArrivalIdRepository]
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          val mockSubmitMessageService      = mock[SubmitMessageService]

          val arrivalId = ArrivalId(1)

          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

          when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
          when(mockSubmitMessageService.submitArrival(any())(any())).thenReturn(Future.successful(SubmissionResult.FailureExternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual BAD_GATEWAY
          }
        }

        "must return BadRequest if the payload is malformed" in {
          val mockArrivalIdRepository       = mock[ArrivalIdRepository]
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

          val application =
            baseApplicationBuilder
              .overrides(
                bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
              )
              .build()

          running(application) {
            val requestXmlBody =
              <CC007A><HEAHEA></HEAHEA></CC007A>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
            header("Location", result) must not be (defined)
            verify(mockArrivalIdRepository, never()).nextId()
            verify(mockArrivalMovementRepository, never()).insert(any())
          }
        }

        "must return BadRequest if the message is not an arrival notification" in {
          val mockArrivalIdRepository       = mock[ArrivalIdRepository]
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

          val application =
            baseApplicationBuilder
              .overrides(
                bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
              )
              .build()

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          running(application) {
            val requestXmlBody = <InvalidRootNode></InvalidRootNode>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
            header("Location", result) must not be (defined)
            verify(mockArrivalIdRepository, never()).nextId()
            verify(mockArrivalMovementRepository, never()).insert(any())
          }
        }
      }

      "when there has been a previous failed attempt to submit" - {
        "must return Accepted when submit against the existing arrival" in {

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          val mockMessageConnector          = mock[MessageConnector]
          val mockSubmitMessageService      = mock[SubmitMessageService]
          val mockLockRepository            = mock[LockRepository]

          when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
          when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))
          when(mockSubmitMessageService.submitMessage(any(), any(), any())(any())).thenReturn(Future.successful(SubmissionResult.Success))

          val application = baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual ACCEPTED
            header("Location", result).value must be(routes.MovementsController.getArrival(arrival.arrivalId).url)
          }
        }

        "must return InternalServerError if there was an internal failure when saving and sending" in {
          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          val mockLockRepository            = mock[LockRepository]
          val mockSubmitMessageService      = mock[SubmitMessageService]

          when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
          when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

          when(mockSubmitMessageService.submitMessage(any(), any(), any())(any())).thenReturn(Future.successful(SubmissionResult.FailureInternal))

          val application = baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService)
            )
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

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
          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

          when(mockSubmitMessageService.submitMessage(any(), any(), any())(any())).thenReturn(Future.successful(SubmissionResult.FailureExternal))

          val app = baseApplicationBuilder
            .overrides(
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector),
              bind[LockRepository].toInstance(mockLockRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService)
            )
            .build()

          running(app) {

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

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
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
              )
              .build()

          running(application) {
            val requestXmlBody = <CC007A><HEAHEA></HEAHEA></CC007A>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

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
          when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(
                bind[LockRepository].toInstance(mockLockRepository),
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
                bind[MessageConnector].toInstance(mockMessageConnector)
              )
              .build()

          running(application) {

            val requestXmlBody = <InvalidRootNode></InvalidRootNode>

            val request = FakeRequest(POST, routes.MovementsController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual BAD_REQUEST
          }
        }
      }
    }

    "putArrival" - {

      "must return Accepted, when the result of submission is a Success" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any())(any())).thenReturn(Future.successful(SubmissionResult.Success))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService)
          )
          .build()

        running(application) {

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)
          val result  = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MovementsController.getArrival(arrival.arrivalId).url)
          verify(mockSubmitMessageService, times(1)).submitMessage(eqTo(arrival.arrivalId), eqTo(messageId), eqTo(movementMessge))(any())
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

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC007A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(ArrivalId(1)).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual NOT_FOUND

        }
      }

      "must return InternalServerError if there was an internal failure when saving and sending" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]
        val mockSubmitMessageService      = mock[SubmitMessageService]

        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(eqTo(arrival.arrivalId), eqTo(messageId), eqTo(movementMessge))(any()))
          .thenReturn(Future.successful(SubmissionResult.Success))

        val application = baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
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
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

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
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockSubmitMessageService.submitMessage(any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionResult.FailureExternal))

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
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

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
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[LockRepository].toInstance(mockLockRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC007A><HEAHEA></HEAHEA></CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

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
          val requestXmlBody = <InvalidRootNode></InvalidRootNode>

          val request = FakeRequest(PUT, routes.MovementsController.putArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
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
              when(mockArrivalMovementRepository.fetchAllArrivals(any())).thenReturn(Future.successful(arrivals))

              val request = FakeRequest(GET, routes.MovementsController.getArrivals().url)

              val result = route(application, request).value

              status(result) mustEqual OK
              contentAsJson(result) mustEqual Json.toJson(Arrivals(arrivals))

              reset(mockArrivalMovementRepository)
          }
        }
      }

      "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the Arrival Movements" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        when(mockArrivalMovementRepository.fetchAllArrivals(any()))
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

    "getMessage" - {
      "must return Ok with the retrieved message and state SubmissionSuccessful" in {

        val message = Arbitrary.arbitrary[MovementMessageWithState].sample.value.copy(state = SubmissionSucceeded)
        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = Seq(message), eoriNumber = "eori")

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        when(mockArrivalMovementRepository.get(any()))
          .thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getMessage(arrival.arrivalId, 0).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(ResponseMovementMessage.build(arrival.arrivalId, 0, message))
        }
      }

      "must return Ok with the retrieved message when it has no state" in {
        val message = Arbitrary.arbitrary[MovementMessageWithoutState].sample.value
        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = Seq(message), eoriNumber = "eori")

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        when(mockArrivalMovementRepository.get(any()))
          .thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MovementsController.getMessage(arrival.arrivalId, 0).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(ResponseMovementMessage.build(arrival.arrivalId, 0, message))
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
            val request = FakeRequest(GET, routes.MovementsController.getMessage(ArrivalId(1), 0).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when message does not exist" in {
          val message = Arbitrary.arbitrary[MovementMessageWithState].sample.value.copy(state = SubmissionSucceeded)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = Seq(message), eoriNumber = "eori")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MovementsController.getMessage(arrival.arrivalId, 5).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when status is not equal to successful" in {
          val message = Arbitrary.arbitrary[MovementMessageWithState].sample.value.copy(state = SubmissionFailed)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = Seq(message), eoriNumber = "eori")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MovementsController.getMessage(arrival.arrivalId, 0).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

        "when message belongs to an arrival the user cannot access" in {
          val message = Arbitrary.arbitrary[MovementMessageWithState].sample.value.copy(state = SubmissionSucceeded)
          val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(messages = Seq(message), eoriNumber = "eori2")

          val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
          when(mockArrivalMovementRepository.get(any()))
            .thenReturn(Future.successful(Some(arrival)))

          val application =
            baseApplicationBuilder
              .overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository))
              .build()

          running(application) {
            val request = FakeRequest(GET, routes.MovementsController.getMessage(arrival.arrivalId, 0).url)
            val result  = route(application, request).value

            status(result) mustEqual NOT_FOUND
          }
        }

      }
    }

  }
}
