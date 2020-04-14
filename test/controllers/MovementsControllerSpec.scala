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
import java.time.LocalTime

import base.SpecBase
import connectors.MessageConnector
import generators.MessageGenerators
import models.Arrival
import models.ArrivalId
import models.Arrivals
import models.MessageType
import models.ArrivalState
import models.MessageState.SubmissionFailed
import models.TransitWrapper
import org.mockito.Matchers.any
import org.mockito.Matchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalIdRepository
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.{HttpException, HttpResponse, Upstream5xxResponse}
import utils.Format

import scala.concurrent.Future
import scala.util.Success

class MovementsControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach with IntegrationPatience {

  "MovementsController" - {

    "createMovement" - {

      "must return Accepted, create movement, send the message upstream and set the state to Submitted" in {
        val mockArrivalIdRepository       = mock[ArrivalIdRepository]
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val arrivalId                     = ArrivalId(1)

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful((Success(()))))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
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

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(arrivalId.index.toString)
          verify(mockArrivalMovementRepository, times(1)).insert(any())
          verify(mockMessageConnector, times(1)).post(eqTo(TransitWrapper(requestXmlBody)), eqTo(MessageType.ArrivalNotification), any(), any())(any())
          verify(mockArrivalMovementRepository, times(1)).setState(any(), eqTo(ArrivalState.ArrivalSubmitted))
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

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be defined
          verify(mockArrivalMovementRepository, times(0)).insert(any())
        }
      }

      "must return InternalServerError if the database fails to create a new Arrival Movement" in {
        val mockArrivalIdRepository       = mock[ArrivalIdRepository]
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val arrivalId                     = ArrivalId(1)

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
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

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be defined
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockArrivalIdRepository       = mock[ArrivalIdRepository]
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector)
            )
            .build()

        running(application) {
          val requestXmlBody =
            <CC007A>
              <HEAHEA>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be (defined)
          verify(mockArrivalIdRepository, times(0)).nextId()
          verify(mockArrivalMovementRepository, times(0)).insert(any())
        }
      }

      "must return BadRequest if the message is not an arrival notification" in {
        val mockArrivalIdRepository       = mock[ArrivalIdRepository]
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector)
            )
            .build()

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        running(application) {
          val requestXmlBody =
            <InvalidRootNode>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </InvalidRootNode>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be (defined)
          verify(mockArrivalIdRepository, times(0)).nextId()
          verify(mockArrivalMovementRepository, times(0)).insert(any())
        }
      }

      "must return InternalServerError and update the message status to SubmissionFailed if sending the message upstream fails" in {

        val mockArrivalIdRepository       = mock[ArrivalIdRepository]
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val arrivalId                     = ArrivalId(1)

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(None))

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful((Success(()))))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.failed(Upstream5xxResponse("message", 500, 500)))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
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

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must return BadGateway" - {
        "must return BadGateway and update the status to SubmissionFailed if sending the message upstream fails" in {

          val errorResponseCode5xx: Gen[Int] = Gen.choose(500, 599)

          forAll(errorResponseCode5xx) {
            responseCode =>
              val mockArrivalIdRepository       = mock[ArrivalIdRepository]
              val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
              val mockMessageConnector          = mock[MessageConnector]
              val arrivalId                     = ArrivalId(1)

              when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrivalId))
              when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
              when(mockArrivalMovementRepository.setState(any(), any())).thenReturn(Future.successful(()))
              when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.failed(new HttpException("Could not submit to EIS", responseCode)))

              val application = baseApplicationBuilder
                .overrides(
                  bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
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

                val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

                val result = route(application, request).value

                status(result) mustEqual BAD_GATEWAY
                verify(mockArrivalMovementRepository, times(1)).setMessageState(arrivalId, any(), SubmissionFailed)
              }
          }
        }
      }

      "must return SeeOther if Arrival Movement already exists" in {
        val mockArrivalIdRepository       = mock[ArrivalIdRepository]
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]

        val arrival = Arbitrary.arbitrary[Arrival].sample.value

        when(mockArrivalMovementRepository.get(any(), any())).thenReturn(Future.successful(Some(arrival)))

        when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(arrival.arrivalId))
        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository),
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
                <DocNumHEA5>{arrival.movementReferenceNumber}</DocNumHEA5>
              </HEAHEA>
            </CC007A>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual SEE_OTHER
          header("Location", result) mustBe Some(s"/movements/arrivals/${arrival.arrivalId}")
        }
      }
    }

    "updateArrival" - {

      "must return Accepted, add the message to the movement, send the message upstream and set the state to Submitted" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addMessage(eqTo(arrival.arrivalId), any(), any())).thenReturn(Future.successful(Success(())))
        when(mockArrivalMovementRepository.setState(any(), any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))

        val application = baseApplicationBuilder
          .overrides(
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

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(arrival.arrivalId.index.toString)
          verify(mockArrivalMovementRepository, times(1)).addMessage(any(), any(), any())
          verify(mockMessageConnector, times(1)).post(eqTo(TransitWrapper(requestXmlBody)), eqTo(MessageType.ArrivalNotification), any(), any())(any())
          verify(mockArrivalMovementRepository, times(1)).setState(any(), eqTo(ArrivalState.ArrivalSubmitted))
        }
      }

      "must return NotFound if there is no Arrival Movement for that ArrivalId" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
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

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(ArrivalId(1)).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual NOT_FOUND

        }
      }

      "must return InternalServerError if the database fails to amend the Arrival Movement" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addMessage(eqTo(arrival.arrivalId), any(), any())).thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
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

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR

        }
      }

      "must return InternalServerError if the database fails to update the message status to sent" ignore {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addMessage(eqTo(arrival.arrivalId), any(), any())).thenReturn(Future.successful(Success(())))
        //when(mockArrivalMovementRepository.setMessageState(eqTo(arrival.arrivalId, eqTo(2)))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
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

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR

        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC007A><HEAHEA></HEAHEA></CC007A>

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message is not an arrival notification" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]

        val arrival = Arbitrary.arbitrary[Arrival].sample.value
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
              bind[MessageConnector].toInstance(mockMessageConnector)
            )
            .build()

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        running(application) {
          val requestXmlBody =
            <InvalidRootNode>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </InvalidRootNode>

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return InternalServerError and update the status to SubmissionFailed if sending the message upstream fails" ignore {

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.addMessage(eqTo(arrival.arrivalId), any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.failed(Upstream5xxResponse("message", 500, 500)))

        val application = baseApplicationBuilder
          .overrides(
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

          val request = FakeRequest(PUT, routes.MovementsController.updateArrival(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          verify(mockArrivalMovementRepository, times(1)).addMessage(arrival.arrivalId, any(), any())
//          verify(mockArrivalMovementRepository, times(1)).setMessageState(any(), any())
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
          forAll(seqWithMaxLength[Arrival](10)) {
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

  }
}
