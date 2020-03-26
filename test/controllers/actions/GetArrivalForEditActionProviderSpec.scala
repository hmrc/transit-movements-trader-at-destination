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

package controllers.actions

import generators.MessageGenerators
import models.Arrival
import models.request.ArrivalId
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository

import scala.concurrent.Future

class GetArrivalForEditActionProviderSpec
    extends FreeSpec
    with MustMatchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(getAndLock: GetArrivalForEditActionProvider) {

    def get(arrivalId: ArrivalId): Action[AnyContent] = getAndLock(arrivalId) {
      request =>
        Results.Ok(request.arrival.arrivalId.toString)
    }
  }

  "get arrival for edit" - {

    "must lock an arrival, retrieve it, then unlock it when the arrival exists" in {

      val arrival = arbitrary[Arrival].sample.value

      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockLockRepository            = mock[LockRepository]

      when(mockArrivalMovementRepository.get(any())) thenReturn Future.successful(Some(arrival))
      when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
      when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetArrivalForEditActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.get(arrival.arrivalId)(fakeRequest)

      status(result) mustEqual OK
      contentAsString(result) mustEqual arrival.arrivalId.toString
      verify(mockLockRepository, times(1)).lock(eqTo(arrival.arrivalId))
      verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
    }

    "must lock an arrival, unlock it, and return Not Found when the arrival cannot be found" in {

      val arrival = arbitrary[Arrival].sample.value

      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockLockRepository            = mock[LockRepository]

      when(mockArrivalMovementRepository.get(any())) thenReturn Future.successful(None)
      when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
      when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetArrivalForEditActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.get(arrival.arrivalId)(fakeRequest)

      status(result) mustEqual NOT_FOUND
      verify(mockLockRepository, times(1)).lock(eqTo(arrival.arrivalId))
      verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
    }

    "must return Locked if a lock cannot be acquired" in {

      val arrival = arbitrary[Arrival].sample.value

      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockLockRepository            = mock[LockRepository]

      when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetArrivalForEditActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.get(arrival.arrivalId)(fakeRequest)

      status(result) mustEqual LOCKED
      verify(mockLockRepository, never).unlock(any())
      verify(mockArrivalMovementRepository, never).get(any())
    }
  }
}
