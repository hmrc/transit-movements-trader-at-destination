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

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.Materializer
import generators.ModelGenerators
import models.Arrival
import models.LockResult.AlreadyLocked
import models.LockResult.LockAcquired
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.atLeastOnce
import org.mockito.Mockito.never
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import repositories.WorkerLockRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class AddJsonToMessagesWorkerSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaFutures
    with OptionValues
    with IntegrationPatience
    with ModelGenerators {

  implicit private val actorSystem: ActorSystem = ActorSystem()
  implicit private val mat: Materializer        = ActorMaterializer()

  "AddJsonToMessagesWorker" - {

    "when the worker is enabled in config" - {

      "when a lock for the overall worker can be obtained" - {

        "and locks on the arrivals can be obtained" - {

          "must update the messages on the items it has to process" in new Fixture {

            private val arrival1 = arbitrary[Arrival].sample.value
            private val arrival2 = arbitrary[Arrival].sample.value

            when(workerConfig.addJsonToMessagesWorkerSettings) thenReturn defaultSettings
            when(lockRepo.lock(any())) thenReturn Future.successful(LockAcquired)
            when(lockRepo.unlock(any())) thenReturn Future.successful(true)
            when(arrivalsRepo.arrivalsWithoutJsonMessages(any())) thenReturn Future.successful(Seq(arrival1, arrival2))
            when(arrivalsRepo.resetMessages(any(), any())) thenReturn Future.successful(true)
            when(arrivalsLockRepo.lock(any())) thenReturn Future.successful(true)
            when(arrivalsLockRepo.unlock(any())) thenReturn Future.successful(true)

            val worker: AddJsonToMessagesWorker = new AddJsonToMessagesWorker(workerConfig, lockRepo, arrivalsRepo, arrivalsLockRepo)

            val result = worker.tap.pull.futureValue.value
            result.size mustEqual 2

            verify(lockRepo, atLeastOnce).lock(lockName)
            verify(lockRepo, times(1)).unlock(lockName)
            verify(arrivalsLockRepo, times(1)).lock(arrival1.arrivalId)
            verify(arrivalsLockRepo, times(1)).lock(arrival2.arrivalId)
            verify(arrivalsLockRepo, times(1)).unlock(arrival1.arrivalId)
            verify(arrivalsLockRepo, times(1)).unlock(arrival2.arrivalId)
            verify(arrivalsRepo, times(1)).resetMessages(arrival1.arrivalId, arrival1.messages)
            verify(arrivalsRepo, times(1)).resetMessages(arrival2.arrivalId, arrival2.messages)
          }

          "and an error occurs trying to update the messages" - {

            "must unlock the arrival and proceed to process remaining items" in new Fixture {

              private val arrival1 = arbitrary[Arrival].sample.value
              private val arrival2 = arbitrary[Arrival].sample.value

              when(workerConfig.addJsonToMessagesWorkerSettings) thenReturn defaultSettings
              when(lockRepo.lock(any())) thenReturn Future.successful(LockAcquired)
              when(lockRepo.unlock(any())) thenReturn Future.successful(true)
              when(arrivalsRepo.arrivalsWithoutJsonMessages(any())) thenReturn Future.successful(Seq(arrival1, arrival2))
              when(arrivalsRepo.resetMessages(any(), any())) thenReturn Future.failed(new Exception("foo")) thenReturn Future.successful(true)
              when(arrivalsLockRepo.lock(any())) thenReturn Future.successful(true)
              when(arrivalsLockRepo.unlock(any())) thenReturn Future.successful(true)

              val worker: AddJsonToMessagesWorker = new AddJsonToMessagesWorker(workerConfig, lockRepo, arrivalsRepo, arrivalsLockRepo)

              val result = worker.tap.pull.futureValue.value
              result.size mustEqual 2

              verify(lockRepo, atLeastOnce).lock(lockName)
              verify(lockRepo, times(1)).unlock(lockName)
              verify(arrivalsLockRepo, times(1)).lock(arrival1.arrivalId)
              verify(arrivalsLockRepo, times(1)).lock(arrival2.arrivalId)
              verify(arrivalsLockRepo, times(1)).unlock(arrival1.arrivalId)
              verify(arrivalsLockRepo, times(1)).unlock(arrival2.arrivalId)
              verify(arrivalsRepo, times(1)).resetMessages(arrival1.arrivalId, arrival1.messages)
              verify(arrivalsRepo, times(1)).resetMessages(arrival2.arrivalId, arrival2.messages)
            }
          }
        }

        "and a lock on the arrival cannot be obtained" - {

          "must not update the arrival's messages" in new Fixture {

            private val arrival1 = arbitrary[Arrival].sample.value
            private val arrival2 = arbitrary[Arrival].sample.value

            when(workerConfig.addJsonToMessagesWorkerSettings) thenReturn defaultSettings
            when(lockRepo.lock(any())) thenReturn Future.successful(LockAcquired)
            when(lockRepo.unlock(any())) thenReturn Future.successful(true)
            when(arrivalsRepo.arrivalsWithoutJsonMessages(any())) thenReturn Future.successful(Seq(arrival1, arrival2))
            when(arrivalsRepo.resetMessages(any(), any())) thenReturn Future.successful(true)
            when(arrivalsLockRepo.lock(any())) thenReturn Future.successful(false)
            when(arrivalsLockRepo.unlock(any())) thenReturn Future.successful(true)

            val worker: AddJsonToMessagesWorker = new AddJsonToMessagesWorker(workerConfig, lockRepo, arrivalsRepo, arrivalsLockRepo)

            val result = worker.tap.pull.futureValue.value
            result.size mustEqual 2

            verify(lockRepo, atLeastOnce).lock(lockName)
            verify(lockRepo, times(1)).unlock(lockName)
            verify(arrivalsLockRepo, times(1)).lock(arrival1.arrivalId)
            verify(arrivalsLockRepo, times(1)).lock(arrival2.arrivalId)
            verify(arrivalsLockRepo, never).unlock(arrival1.arrivalId)
            verify(arrivalsLockRepo, never).unlock(arrival2.arrivalId)
            verify(arrivalsRepo, never).resetMessages(arrival1.arrivalId, arrival1.messages)
            verify(arrivalsRepo, never).resetMessages(arrival2.arrivalId, arrival2.messages)
          }
        }
      }

      "when a lock for the overall worker cannot be obtained" - {

        "must not process any items" in new Fixture {

          private val arrival = arbitrary[Arrival].sample.value

          when(workerConfig.addJsonToMessagesWorkerSettings) thenReturn defaultSettings
          when(lockRepo.lock(any())) thenReturn Future.successful(AlreadyLocked)
          when(lockRepo.unlock(any())) thenReturn Future.successful(true)
          when(arrivalsRepo.arrivalsWithoutJsonMessages(any())) thenReturn Future.successful(Seq(arrival))
          when(arrivalsRepo.resetMessages(any(), any())) thenReturn Future.successful(true)
          when(arrivalsLockRepo.lock(any())) thenReturn Future.successful(false)

          val worker: AddJsonToMessagesWorker = new AddJsonToMessagesWorker(workerConfig, lockRepo, arrivalsRepo, arrivalsLockRepo)

          worker.tap.pull.isReadyWithin(1.second) mustEqual false

          verify(arrivalsLockRepo, never).lock(arrival.arrivalId)
          verify(arrivalsLockRepo, never).unlock(arrival.arrivalId)
          verify(arrivalsRepo, never).arrivalsWithoutJsonMessages(any())
          verify(arrivalsRepo, never).resetMessages(any(), any())
          verify(lockRepo, never).unlock(any())
        }
      }
    }

    "when the worker is disabled in config" - {

      "must not process any items" in new Fixture {

        private val arrival = arbitrary[Arrival].sample.value

        val settings = defaultSettings copy (enabled = false)
        when(workerConfig.addJsonToMessagesWorkerSettings) thenReturn settings
        when(lockRepo.lock(any())) thenReturn Future.successful(AlreadyLocked)
        when(lockRepo.unlock(any())) thenReturn Future.successful(true)
        when(arrivalsRepo.arrivalsWithoutJsonMessages(any())) thenReturn Future.successful(Seq(arrival))
        when(arrivalsRepo.resetMessages(any(), any())) thenReturn Future.successful(true)
        when(arrivalsLockRepo.lock(any())) thenReturn Future.successful(true)
        when(arrivalsLockRepo.unlock(any())) thenReturn Future.successful(true)

        val worker: AddJsonToMessagesWorker = new AddJsonToMessagesWorker(workerConfig, lockRepo, arrivalsRepo, arrivalsLockRepo)

        val result = worker.tap.pull.futureValue
        result must not be defined

        verify(lockRepo, never).lock(any())
        verify(lockRepo, never).unlock(any())
        verify(arrivalsRepo, never).arrivalsWithoutJsonMessages(any())
        verify(arrivalsRepo, never).resetMessages(any(), any())
      }
    }
  }

  trait Fixture {
    protected val workerConfig: WorkerConfig              = mock[WorkerConfig]
    protected val lockRepo: WorkerLockRepository          = mock[WorkerLockRepository]
    protected val arrivalsRepo: ArrivalMovementRepository = mock[ArrivalMovementRepository]
    protected val arrivalsLockRepo: LockRepository        = mock[LockRepository]

    protected val lockName: String = "add-json-to-messages-worker"

    protected val defaultSettings: WorkerSettings = WorkerSettings(
      interval = 2.seconds,
      groupSize = 20,
      parallelism = 32,
      elements = 1,
      per = 1.second,
      enabled = true
    )
  }
}
