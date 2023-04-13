/*
 * Copyright 2023 HM Revenue & Customs
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

package services

import base.SpecBase
import generators.ModelGenerators
import models._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.LockRepository

import scala.concurrent.Future

class LockServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockLockRepository = mock[LockRepository]

  "LockService" - {

    "lock" - {

      "must add a lock when given an arrival Id" in {

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder.overrides(bind[LockRepository].toInstance(mockLockRepository)).build()

        running(application) {
          val service = application.injector.instanceOf[LockService]

          val result = service.lock(ArrivalId(0)).futureValue.value

          result mustBe (())
        }
      }

      "must return a DocumentExistsError if the lock is already there" in {

        when(mockLockRepository.lock(any())).thenReturn(Future.successful(false))

        val application = baseApplicationBuilder.overrides(bind[LockRepository].toInstance(mockLockRepository)).build()

        running(application) {
          val service = application.injector.instanceOf[LockService]

          val result = service.lock(ArrivalId(0)).futureValue.left.value

          result mustBe an[DocumentExistsError]
        }
      }

      "must return a FailedToLockService if the lock fails and attempt to unlock" in {

        when(mockLockRepository.lock(any())).thenReturn(Future.failed(new Exception))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder.overrides(bind[LockRepository].toInstance(mockLockRepository)).build()

        running(application) {
          val service = application.injector.instanceOf[LockService]

          val result = service.lock(ArrivalId(0)).futureValue.left.value

          result mustBe an[FailedToLock]
        }
      }
    }

    "unlock" - {

      "must add an unlock when given an Arrival Id" in {

        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(true))

        val application = baseApplicationBuilder.overrides(bind[LockRepository].toInstance(mockLockRepository)).build()

        running(application) {
          val service = application.injector.instanceOf[LockService]

          val result = service.unlock(ArrivalId(0)).futureValue.value

          result mustBe (())
        }
      }

      "must return a FailedToUnlockService if the unlock fails" in {

        when(mockLockRepository.unlock(any())).thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder.overrides(bind[LockRepository].toInstance(mockLockRepository)).build()

        running(application) {
          val service = application.injector.instanceOf[LockService]

          val result = service.unlock(ArrivalId(0)).futureValue.left.value

          result mustBe an[FailedToUnlock]
        }
      }
    }

  }
}
