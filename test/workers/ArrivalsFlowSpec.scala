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

package workers

import akka.Done
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.LockResult.AlreadyLocked
import models.LockResult.LockAcquired
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import repositories.ArrivalMovementRepository

import scala.concurrent.Future

class ArrivalsFlowSpec extends AnyFreeSpec with Matchers with MockitoSugar with ScalaFutures with OptionValues with IntegrationPatience with ModelGenerators {

  implicit private val actorSystem: ActorSystem = ActorSystem()

  "when the lock has been acquired (LockAcquired), emits values from the substream of arrivals" in {
    val arrival = arbitrary[Arrival].sample.value

    val arrivalsGroup1 = List(
      arrival.copy(arrivalId = ArrivalId(1)),
      arrival.copy(arrivalId = ArrivalId(2)),
      arrival.copy(arrivalId = ArrivalId(3))
    )

    val arrivalsGroup2 = List(
      arrival.copy(arrivalId = ArrivalId(4)),
      arrival.copy(arrivalId = ArrivalId(5)),
      arrival.copy(arrivalId = ArrivalId(6))
    )

    val source1 = Source(arrivalsGroup1).mapMaterializedValue(
      _ => Future.successful(Done)
    )
    val source2 = Source(arrivalsGroup2).mapMaterializedValue(
      _ => Future.successful(Done)
    )

    val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

    when(mockArrivalMovementRepository.arrivalsWithoutJsonMessagesSource(any()))
      .thenReturn(Future.successful(source1))
      .thenReturn(Future.successful(source2))

    val mockWorkerSettings = mock[WorkerSettings]
    when(mockWorkerSettings.groupSize).thenReturn(3)
    val mockWorkerConfig = mock[WorkerConfig]
    when(mockWorkerConfig.addJsonToMessagesWorkerSettings).thenReturn(mockWorkerSettings)

    val flowUnderTest = new ArrivalsFlow(mockWorkerConfig, mockArrivalMovementRepository)

    Source(List(LockAcquired, LockAcquired))
      .via(flowUnderTest())
      .runWith(TestSink.probe[Arrival])
      .request(n = 6)
      .expectNextN(arrivalsGroup1 ++ arrivalsGroup2)
      .expectComplete()

  }

  "when already locked (AlreadyLocked), no values are emitted from the substream of arrivals" in {
    val arrival = arbitrary[Arrival].sample.value

    val arrivalsGroup1 = List(
      arrival.copy(arrivalId = ArrivalId(1)),
      arrival.copy(arrivalId = ArrivalId(2)),
      arrival.copy(arrivalId = ArrivalId(3))
    )

    val source1 = Source(arrivalsGroup1).mapMaterializedValue(
      _ => Future.successful(Done)
    )

    val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

    when(mockArrivalMovementRepository.arrivalsWithoutJsonMessagesSource(any()))
      .thenReturn(Future.successful(source1))

    val mockWorkerSettings = mock[WorkerSettings]
    when(mockWorkerSettings.groupSize).thenReturn(3)
    val mockWorkerConfig = mock[WorkerConfig]
    when(mockWorkerConfig.addJsonToMessagesWorkerSettings).thenReturn(mockWorkerSettings)

    val flowUnderTest = new ArrivalsFlow(mockWorkerConfig, mockArrivalMovementRepository)

    Source(List(AlreadyLocked))
      .via(flowUnderTest())
      .runWith(TestSink.probe[Arrival])
      .request(n = 1)
      .expectComplete()

  }

  "when there is is a mix of LockAcquired and AlreadyLocked, no values are emitted for AlreadyLocked" in {
    val arrival = arbitrary[Arrival].sample.value

    val arrivalsGroup1 = List(
      arrival.copy(arrivalId = ArrivalId(1)),
      arrival.copy(arrivalId = ArrivalId(2)),
      arrival.copy(arrivalId = ArrivalId(3))
    )

    val arrivalsGroup2 = List(
      arrival.copy(arrivalId = ArrivalId(4)),
      arrival.copy(arrivalId = ArrivalId(5)),
      arrival.copy(arrivalId = ArrivalId(6))
    )

    val arrivalsGroup3 = List(
      arrival.copy(arrivalId = ArrivalId(4)),
      arrival.copy(arrivalId = ArrivalId(5)),
      arrival.copy(arrivalId = ArrivalId(6))
    )

    val source1 = Source(arrivalsGroup1).mapMaterializedValue(
      _ => Future.successful(Done)
    )
    val source2 = Source(arrivalsGroup2).mapMaterializedValue(
      _ => Future.successful(Done)
    )
    val source3 = Source(arrivalsGroup3).mapMaterializedValue(
      _ => Future.successful(Done)
    )

    val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

    when(mockArrivalMovementRepository.arrivalsWithoutJsonMessagesSource(any()))
      .thenReturn(Future.successful(source1))
      .thenReturn(Future.successful(source2))
      .thenReturn(Future.successful(source3))

    val mockWorkerSettings = mock[WorkerSettings]
    when(mockWorkerSettings.groupSize).thenReturn(3)
    val mockWorkerConfig = mock[WorkerConfig]
    when(mockWorkerConfig.addJsonToMessagesWorkerSettings).thenReturn(mockWorkerSettings)

    val flowUnderTest = new ArrivalsFlow(mockWorkerConfig, mockArrivalMovementRepository)

    Source(List(LockAcquired, AlreadyLocked, LockAcquired))
      .via(flowUnderTest())
      .runWith(TestSink.probe[Arrival])
      .request(n = 7)
      .expectNextN(arrivalsGroup1 ++ arrivalsGroup2)
      .expectComplete()

  }
}
