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

import akka.Done
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.MovementMessage
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.listOfN
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import repositories.WorkerLockRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class AddJsonToMessagesTransformerSpec extends SpecBase with ModelGenerators with ScalaCheckPropertyChecks {

  implicit private val actorSystem: ActorSystem = ActorSystem()
  implicit private val mat: Materializer        = ActorMaterializer()

  implicit val arbArrival: Gen[Arrival] =
    for {
      arrival  <- arbitraryArrival.arbitrary
      messages <- nonEmptyListOfMaxLength[MovementMessage](10)
    } yield {
      arrival.copy(messages = messages)
    }

  "the flow" - {
    "adds json for all the messages" in new Fixture {

      val arrivals = listOfN(6, arbitrary[Arrival]).sample.value
      val source   = Source(arrivals)

      when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings)
      when(
        arrivalsRepo.resetMessages(arrivalIdArgumentCaptor.capture(), messagesArgumentCaptor.capture())
      ).thenReturn(Future.successful(true))

      val expectedResultFromStream = arrivals.map(Seq(_))

      source
        .via(addJsonToMessagesTransformer.flow)
        .map(_.map(_._1))
        .runWith(TestSink.probe[Seq[Arrival]])
        .request(7)
        .expectNextN(expectedResultFromStream)
        .expectComplete()

      val expectedCallToRepository = arrivals.map(_.arrivalId)

      arrivalIdArgumentCaptor.getAllValues must contain theSameElementsAs expectedCallToRepository
      messagesArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.messages)

    }

    "groups the messages into configured chunks" in new Fixture {

      val arrivals = listOfN(7, arbitrary[Arrival]).sample.value
      val source   = Source(arrivals)

      val groupSize = 3
      when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings.copy(groupSize = groupSize))
      when(arrivalsRepo.resetMessages(any(), any())).thenReturn(Future.successful(true))

      val expectedResultFromStream = arrivals.map(Seq(_))

      val result: Seq[Seq[Arrival]] = source
        .via(addJsonToMessagesTransformer.flow)
        .map(_.map(_._1))
        .runWith(TestSink.probe[Seq[Arrival]])
        .request(4)
        .expectNextN(3)

      result must contain theSameElementsInOrderAs arrivals.grouped(groupSize).toList

    }
  }

  abstract class Fixture {

    protected val defaultSettings: WorkerSettings = WorkerSettings(
      interval = 2.seconds,
      groupSize = 1,
      parallelism = 32,
      elements = 1,
      per = 1.second,
      enabled = true
    )

    protected val workerConfig: WorkerConfig     = mock[WorkerConfig]
    protected val lockRepo: WorkerLockRepository = mock[WorkerLockRepository]

    protected val arrivalsRepo: ArrivalMovementRepository = mock[ArrivalMovementRepository]

    val arrivalIdArgumentCaptor = ArgumentCaptor.forClass(classOf[ArrivalId])
    val messagesArgumentCaptor  = ArgumentCaptor.forClass(classOf[NonEmptyList[MovementMessage]])

    protected val arrivalsLockRepo: LockRepository = mock[LockRepository]
    when(arrivalsLockRepo.lock(any())).thenReturn(Future.successful(true))
    when(arrivalsLockRepo.unlock(any())).thenReturn(Future.successful(true))

    def sourceOfElements[A](elements: List[A]): Source[A, Future[Done]] =
      Source(elements).mapMaterializedValue(_ => Future.successful(Done))

    def addJsonToMessagesTransformer = new AddJsonToMessagesTransformer(workerConfig, arrivalsRepo, arrivalsLockRepo)
  }
}
