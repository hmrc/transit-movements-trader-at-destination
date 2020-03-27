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
import models.MessageSender
import models.State
import models.request.ArrivalId
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class GoodsReleasedControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators {

  "post" - {

    "when a lock can be acquired" - {

      "must lock the arrival, add the message, set the state to Goods Released, unlock it and return OK" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        val arrivalId     = ArrivalId(1)
        val version       = 1
        val messageSender = MessageSender(arrivalId, version)
        val arrival = Arrival(
          arrivalId,
          "mrn",
          "eori",
          State.Submitted,
          Seq.empty
        )

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
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

          status(result) mustEqual OK
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockArrivalMovementRepository, times(1)).addMessage(any(), any(), eqTo(State.GoodsReleased))
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return NotFound and unlock when given a message for an arrival that does not exist" in {
        val arrivalId     = ArrivalId(1)
        val messageSender = MessageSender(arrivalId, 1)

        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
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
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return Internal Server Error and release the lock if the message is not Goods Released" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        val arrivalId     = ArrivalId(1)
        val version       = 1
        val messageSender = MessageSender(arrivalId, version)
        val arrival = Arrival(
          arrivalId,
          "mrn",
          "eori",
          State.Submitted,
          Seq.empty
        )

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful())

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <InvalidRootNode>
              <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
              <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            </InvalidRootNode>

          val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          verify(mockArrivalMovementRepository, never).addMessage(any(), any(), any())
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return Internal Server Error and unlock if adding the message to the movement fails" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        val arrivalId     = ArrivalId(1)
        val version       = 1
        val messageSender = MessageSender(arrivalId, version)
        val arrival = Arrival(
          arrivalId,
          "mrn",
          "eori",
          State.Submitted,
          Seq.empty
        )

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addMessage(any(), any(), any())).thenReturn(Future.successful(Failure(new Exception())))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
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
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }
    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {
        val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
        val mockLockRepository            = mock[LockRepository]

        val arrivalId     = ArrivalId(1)
        val version       = 1
        val messageSender = MessageSender(arrivalId, version)
        val arrival = Arrival(
          arrivalId,
          "mrn",
          "eori",
          State.Submitted,
          Seq.empty
        )

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(false))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
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

          status(result) mustEqual LOCKED
        }
      }
    }
  }
}
