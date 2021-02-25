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
import akka.stream.Supervision
import akka.stream.scaladsl.Flow
import base.SpecBase
import models.LockResult
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import play.api.Configuration

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class StreamWorkerSpec extends SpecBase {

  implicit private val actorSystem: ActorSystem = ActorSystem()
  implicit private val mat: Materializer        = ActorMaterializer()

  "the flow is run" - {
    "when upstream acquires a lock and passes it downstream to the flow" in new Fixture {
      override val lockResults: Iterator[LockResult] =
        Iterator(
          LockResult.LockAcquired,
          LockResult.LockAcquired,
          LockResult.LockAcquired
        )

      val flow = Flow[LockResult].map(_ => 1)

      val stream = streamWorker(flow).runWithTap.run()

      stream.pull.futureValue.value mustEqual 1
      stream.pull.futureValue.value mustEqual 1
      stream.pull.futureValue.value mustEqual 1
      stream.pull.futureValue must not be (defined)
    }
  }

  "the flow isn't run" - {
    "when upstream does not acquire a lock and passes an already locked signal downstream to the flow" in new Fixture {
      override val lockResults: Iterator[LockResult] =
        Iterator(
          LockResult.AlreadyLocked,
          LockResult.LockAcquired,
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked,
          LockResult.LockAcquired
        )

      val flow = Flow[LockResult].map(_ => 1)

      val stream = streamWorker(flow).runWithTap.run()

      stream.pull.futureValue.value mustEqual 1
      stream.pull.futureValue.value mustEqual 1
      stream.pull.futureValue must not be (defined)
    }

    "and completes with no work being done by the flow, when upstream never acquire a lock" in new Fixture {
      override val lockResults: Iterator[LockResult] =
        Iterator(
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked
        )

      val flow = Flow[LockResult].map(_ => 1)

      val stream = streamWorker(flow).runWithTap.run()

      stream.pull.futureValue must not be (defined)
      numberOfUnlocks mustEqual 0
    }
  }

  "unlocking" - {
    "is never called when  upstream never acquire a lock" in new Fixture {

      override val lockResults: Iterator[LockResult] =
        Iterator(
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked
        )

      val flow = Flow[LockResult].map(_ => 1)

      val stream = streamWorker(flow).runWithTap.run()

      numberOfUnlocks mustEqual 0
      stream.pull.futureValue must not be (defined)
      numberOfUnlocks mustEqual 0
    }

    "matches the number of already locked sent to the flow" in new Fixture {

      override val lockResults: Iterator[LockResult] =
        Iterator(
          LockResult.AlreadyLocked,
          LockResult.LockAcquired,
          LockResult.LockAcquired,
          LockResult.AlreadyLocked,
          LockResult.AlreadyLocked,
          LockResult.LockAcquired
        )

      val flow = Flow[LockResult].map(_ => 1)

      val stream = streamWorker(flow).runWithTap.run()

      numberOfUnlocks mustEqual 0
      stream.pull.futureValue must be(defined)
      stream.pull.futureValue must be(defined)
      stream.pull.futureValue must be(defined)
      stream.pull.futureValue must not be (defined)
      numberOfUnlocks mustEqual 3
    }
  }

  abstract class Fixture {
    val lockResults: Iterator[LockResult]

    var numberOfUnlocks: Int = 0

    protected lazy val workerLockingService: WorkerLockingService = new WorkerLockingService {
      override def hasNext: Boolean = lockResults.hasNext

      override def next(): Future[LockResult] = Future.successful(lockResults.next())

      def releaseLock(): Future[Boolean] = {
        numberOfUnlocks = numberOfUnlocks + 1
        Future.successful(true)
      }
    }

    protected val defaultSettings: WorkerSettings = WorkerSettings(
      interval = 1.milli,
      groupSize = 1,
      parallelism = 32,
      elements = 1,
      per = 1.second,
      enabled = false // Disables the automatic running of the stream for exclusive testing via runWithTap
    )

    protected val playConfig: Configuration = mock[Configuration]
    when(playConfig.get(any())(any())).thenReturn(defaultSettings)

    def streamWorker(testFlow: Flow[LockResult, Int, NotUsed]): StreamWorker[Int] = new StreamWorker[Int](workerLockingService)("test-worker", playConfig) {

      override def flow: Flow[LockResult, Int, NotUsed] = testFlow

      override def customSupervisionStrategy: Option[PartialFunction[Throwable, Supervision.Directive]] = None

    }

  }

}
