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
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.MessageSender
import models.MovementReferenceNumber
import models.ArrivalState
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.XmlValidationService
import uk.gov.hmrc.http.BadRequestException
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class GoodsReleasedControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  private val mockArrivalMovementRepository: ArrivalMovementRepository = mock[ArrivalMovementRepository]
  private val mockLockRepository: LockRepository                       = mock[LockRepository]
  private val mockXmlValidationService: XmlValidationService           = mock[XmlValidationService]

  private val dateOfPrep = LocalDate.now()
  private val timeOfPrep = LocalTime.of(1, 1)

  private val arrivalId     = ArrivalId(1)
  private val version       = 1
  private val messageSender = MessageSender(arrivalId, version)
  private val arrival = Arrival(
    arrivalId,
    MovementReferenceNumber("mrn"),
    "eori",
    ArrivalState.ArrivalSubmitted,
    LocalDateTime.of(dateOfPrep, timeOfPrep),
    LocalDateTime.of(dateOfPrep, timeOfPrep),
    Seq.empty,
    1
  )

  private val requestXmlBody =
    <CC025A>
      <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
    </CC025A>

  override def beforeEach: Unit = {
    super.beforeEach()
    reset(mockArrivalMovementRepository)
    reset(mockLockRepository)
    reset(mockXmlValidationService)
  }

  "post" - {

    "when a lock can be acquired" - {

      "must lock the arrival, add the message, set the state to Goods Released, unlock it and return OK" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[XmlValidationService].toInstance(mockXmlValidationService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual OK
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockArrivalMovementRepository, times(1)).addResponseMessage(any(), any(), eqTo(ArrivalState.GoodsReleased))
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return NotFound and unlock when given a message for an arrival that does not exist" in {
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
          val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual NOT_FOUND
          verify(mockArrivalMovementRepository, never).addResponseMessage(any(), any(), any())
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return Internal Server Error and release the lock if the message is not Goods Released" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[XmlValidationService].toInstance(mockXmlValidationService)
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
          verify(mockArrivalMovementRepository, never).addResponseMessage(any(), any(), any())
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return Internal Server Error and unlock if adding the message to the movement fails" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Failure(new Exception())))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[XmlValidationService].toInstance(mockXmlValidationService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }

      "must lock, return Bad request and unlock if input message fails to validate against the schema" in {
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockXmlValidationService.validate(any(), any())).thenReturn(Failure(new BadRequestException("")))
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[XmlValidationService].toInstance(mockXmlValidationService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          verify(mockLockRepository, times(1)).lock(arrivalId)
          verify(mockLockRepository, times(1)).unlock(arrivalId)
        }
      }
    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {
        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(false))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.GoodsReleasedController.post(messageSender).url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual LOCKED
        }
      }
    }
  }
}
