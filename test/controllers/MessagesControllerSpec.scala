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
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.MessageStatus
import models.MovementMessageWithState
import org.mockito.Matchers.any
import org.mockito.Matchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import uk.gov.hmrc.http.HttpException
import uk.gov.hmrc.http.HttpResponse
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class MessagesControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  "MessagesController" - {

    "post" - {

      "must return Accepted, add the message to the movement, send the message upstream and set the message state to Submitted" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]
        val arrival =
          Arbitrary
            .arbitrary[Arrival]
            .sample
            .value
            .copy(eoriNumber = "eori", messages = Seq(Arbitrary.arbitrary[MovementMessageWithState].sample.value), status = ArrivalStatus.ArrivalSubmitted)

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addNewMessage(eqTo(arrival.arrivalId), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
        when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful(Success(())))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC044A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)
          val result  = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.MessagesController.post(arrival.arrivalId).url)
          verify(mockArrivalMovementRepository, times(1)).addNewMessage(any(), any())
          verify(mockMessageConnector, times(1)).post(any(), any(), any())(any())
          verify(mockArrivalMovementRepository, times(1)).setMessageState(any(), any(), eqTo(MessageStatus.SubmissionSucceeded))
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
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector)
          )
          .build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC044A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(ArrivalId(1)).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual NOT_FOUND

        }
      }

      "must return InternalServerError if the database fails to update the message status to sent" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addNewMessage(eqTo(arrival.arrivalId), any())).thenReturn(Future.successful(Success(())))
        when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful(Failure(new Exception)))

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
            <CC044A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR

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
              bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
            )
            .build()

        running(application) {
          val requestXmlBody = <CC044A><HEAHEA></HEAHEA></CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return NotImplemented if the message is not supported" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]

        val arrival = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
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
            <CC045A>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
              <HEAHEA>
                <DocNumHEA5>MRN</DocNumHEA5>
              </HEAHEA>
            </CC045A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual NOT_IMPLEMENTED
        }
      }

      "must return BadGateway and update the message status to SubmissionFailed if sending the message upstream fails" in {

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockMessageConnector          = mock[MessageConnector]
        val mockLockRepository            = mock[LockRepository]
        val arrival                       = Arbitrary.arbitrary[Arrival].sample.value.copy(eoriNumber = "eori")

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addNewMessage(eqTo(arrival.arrivalId), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any())(any()))
          .thenReturn(Future.failed(new HttpException("Could not submit to EIS", 500)))
        when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful(Success(())))

        val app = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[MessageConnector].toInstance(mockMessageConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(app) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <CC044A>
                  <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
                  <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
                  <HEAHEA>
                    <DocNumHEA5>MRN</DocNumHEA5>
                  </HEAHEA>
                </CC044A>

          val request = FakeRequest(POST, routes.MessagesController.post(arrival.arrivalId).url).withXmlBody(requestXmlBody)

          val result = route(app, request).value

          status(result) mustEqual BAD_GATEWAY
          verify(mockArrivalMovementRepository, times(1)).setMessageState(eqTo(arrival.arrivalId),
                                                                          eqTo(arrival.messages.length),
                                                                          eqTo(MessageStatus.SubmissionFailed))
        }
      }

    }

  }
}
