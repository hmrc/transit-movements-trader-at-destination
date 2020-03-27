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
import generators.ModelGenerators
import models.Arrival
import models.ArrivalDateTime
import models.MessageSender
import models.MovementReferenceNumber
import models.State
import models.request.ArrivalId
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import utils.Format

import scala.concurrent.Future
import scala.util.Success

class GoodsReleasedControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators {

  "post" - {

    "must add the message to the arrival, set the state to Goods Released and return OK" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      val arrivalId     = ArrivalId(1)
      val version       = 1
      val messageSender = MessageSender(arrivalId, version)
      val dateOfPrep    = LocalDate.now()
      val timeOfPrep    = LocalTime.of(1, 1)
      val arrival = Arrival(
        arrivalId,
        MovementReferenceNumber("mrn"),
        "eori",
        State.Submitted,
        ArrivalDateTime(dateOfPrep, timeOfPrep),
        ArrivalDateTime(dateOfPrep, timeOfPrep),
        Seq.empty
      )

      when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
      when(mockArrivalMovementRepository.addMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

        val result = route(application, request).value

        status(result) mustEqual OK
        verify(mockArrivalMovementRepository, times(1)).addMessage(any(), any(), eqTo(State.GoodsReleased))
      }
    }

    "must return NotFound when given a message for an arrival that does not exist" in {
      val messageSender = MessageSender(ArrivalId(1), 1)

      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {
        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

        val result = route(application, request).value

        status(result) mustEqual NOT_FOUND
        verify(mockArrivalMovementRepository, never).addMessage(any(), any(), any())
      }
    }

    "must return Internal Server Error if the message is not Goods Released" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      val arrivalId     = ArrivalId(1)
      val version       = 1
      val messageSender = MessageSender(arrivalId, version)
      val dateOfPrep    = LocalDate.now()
      val timeOfPrep    = LocalTime.of(1, 1)

      val arrival = Arrival(
        arrivalId,
        MovementReferenceNumber("mrn"),
        "eori",
        State.Submitted,
        ArrivalDateTime(dateOfPrep, timeOfPrep),
        ArrivalDateTime(dateOfPrep, timeOfPrep),
        Seq.empty
      )

      when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
      when(mockArrivalMovementRepository.addMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {
        val requestXmlBody =
          <InvalidRootNode>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </InvalidRootNode>

        val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
        verify(mockArrivalMovementRepository, never).addMessage(any(), any(), any())
      }
    }

    "must return Internal Server Error if adding the message to the movement fails" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      val arrivalId     = ArrivalId(1)
      val version       = 1
      val messageSender = MessageSender(arrivalId, version)
      val dateOfPrep    = LocalDate.now()
      val timeOfPrep    = LocalTime.of(1, 1)

      val arrival = Arrival(
        arrivalId,
        MovementReferenceNumber("mrn"),
        "eori",
        State.Submitted,
        ArrivalDateTime(dateOfPrep, timeOfPrep),
        ArrivalDateTime(dateOfPrep, timeOfPrep),
        Seq.empty
      )

      when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
      when(mockArrivalMovementRepository.addMessage(any(), any(), any())).thenReturn(Future.failed(new Exception()))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {
        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }
}
