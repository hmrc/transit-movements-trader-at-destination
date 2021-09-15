/*
 * Copyright 2021 HM Revenue & Customs
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

package workers

import base.SpecBase
import models.LockResult
import org.mockito.Mockito.when
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.mockito.ArgumentMatchers._
import play.api.inject.ApplicationLifecycle
import play.api.inject.DefaultApplicationLifecycle
import repositories.WorkerLockRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class WorkerLockingServiceSpec extends SpecBase with ScalaCheckPropertyChecks {

  abstract class Fixture(workerEnabled: Boolean) {
    private val mockWorkerSettings = mock[WorkerSettings]
    when(mockWorkerSettings.enabled).thenReturn(workerEnabled)

    private val mockWorkerConfig = mock[WorkerConfig]
    when(mockWorkerConfig.addJsonToMessagesWorkerSettings).thenReturn(mockWorkerSettings)

    val mockWorkerLockRepository = mock[WorkerLockRepository]
    val mockApplicationLifecycle = mock[ApplicationLifecycle]

    val workerLockingService = new WorkerLockingServiceImpl(mockWorkerConfig, mockWorkerLockRepository, mockApplicationLifecycle)
  }

  "when the worker is enabled" - {

    "hasNext is true" in new Fixture(true) {

      workerLockingService.hasNext must be(true)

    }

    "next" - {
      "returns a Future of a Lock" in new Fixture(true) {
        forAll(Gen.oneOf(LockResult.values)) {
          lockResult =>
            when(mockWorkerLockRepository.lock(any())).thenReturn(Future.successful(lockResult))

            workerLockingService.next().futureValue mustEqual lockResult
        }

      }

      "returns a Future of an exception if there is an error in acquiring a lock" in new Fixture(true) {

        when(mockWorkerLockRepository.lock(any())).thenReturn(Future.failed(new Exception("couldn't lock :(")))

        val ex = intercept[Exception] {
          workerLockingService.next().futureValue
        }

        ex.getMessage.contains("couldn't lock :(") mustEqual true

      }
    }
  }

  "when the worker is enabled" - {

    "hasNext is false" in new Fixture(false) {

      workerLockingService.hasNext must be(false)
    }

    "next" - {
      "throws a NoSuchElementException" in new Fixture(false) {
        forAll(Gen.oneOf(LockResult.values)) {
          _ =>
            val ex = intercept[NoSuchElementException] {
              workerLockingService.next()
            }

            ex.getMessage.contains("worker is disabled") mustEqual true
        }

      }
    }

  }

  "does not produce more elements when the application is shutting down" in {

    val mockWorkerSettings = mock[WorkerSettings]
    when(mockWorkerSettings.enabled).thenReturn(true)

    val mockWorkerConfig = mock[WorkerConfig]
    when(mockWorkerConfig.addJsonToMessagesWorkerSettings).thenReturn(mockWorkerSettings)

    val mockWorkerLockRepository = mock[WorkerLockRepository]

    val applicationLifecycle = new DefaultApplicationLifecycle()

    val workerLockingService = new WorkerLockingServiceImpl(mockWorkerConfig, mockWorkerLockRepository, applicationLifecycle)

    workerLockingService.hasNext mustEqual true

    applicationLifecycle.stop()

    workerLockingService.hasNext mustEqual false

  }
}
