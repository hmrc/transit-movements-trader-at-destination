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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import repositories.WorkerLockRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class AddJsonToMessagesTransformerSpec extends SpecBase with ModelGenerators with ScalaCheckPropertyChecks {

  implicit private val actorSystem: ActorSystem             = ActorSystem()
  implicit private val mat: Materializer                    = ActorMaterializer()
  implicit protected val executionContext: ExecutionContext = actorSystem.dispatcher

  implicit val arbArrival: Gen[Arrival] =
    for {
      arrival  <- arbitraryArrival.arbitrary
      messages <- nonEmptyListOfMaxLength[MovementMessage](10)
    } yield {
      arrival.copy(messages = messages)
    }

  "the flow" - {
    def listOfArrivals(count: Int): Gen[List[Arrival]] =
      arbitrary[Arrival].map {
        arrival =>
          Iterator
            .range(1, count + 1)
            .map(ArrivalId(_))
            .map(arrivalId => arrival.copy(arrivalId = arrivalId))
            .toList
      }

    "adds json for all the messages" in new Fixture {

      val arrivals = listOfArrivals(6).sample.value

      val source = Source(arrivals)

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

      val arrivals = listOfArrivals(7).sample.value
      val source   = Source(arrivals)

      val groupSize          = 3
      val expectedFlowResult = arrivals.grouped(groupSize).toList

      when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings.copy(groupSize = groupSize))
      when(arrivalsRepo.resetMessages(any(), any())).thenReturn(Future.successful(true))

      val result: Seq[Seq[Arrival]] = source
        .via(addJsonToMessagesTransformer.flow)
        .map(_.map(_._1))
        .runWith(TestSink.probe[Seq[Arrival]])
        .request(4)
        .expectNextN(expectedFlowResult.size.toLong)

      result must contain theSameElementsInOrderAs expectedFlowResult

    }

    "resumes if there is a problem while updating a message" - {

      "when the update of a arrival fails" in new Fixture {
        val arrivals = listOfArrivals(3).sample.value
        val source   = Source(arrivals)

        when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings)
        when(
          arrivalsRepo.resetMessages(arrivalIdArgumentCaptor.capture(), messagesArgumentCaptor.capture())
        ).thenReturn(Future.successful(true))
          .thenReturn(Future.failed(new Exception("did not insert")))
          .thenReturn(Future.successful(true))

        val successfulUpdates: Seq[Seq[Arrival]] = source
          .via(addJsonToMessagesTransformer.flow)
          .map(_.map(_._1))
          .runWith(TestSink.probe[Seq[Arrival]])
          .request(3)
          .expectNextN(2)

        arrivalIdArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.arrivalId)
        successfulUpdates must contain theSameElementsAs (List(List(arrivals(0)), List(arrivals(2))))
        messagesArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.messages)

      }

      "when the arrival lock cannot be acquired" in new Fixture {
        val arrivals = listOfArrivals(3).sample.value
        val source   = Source(arrivals)

        when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings)
        when(arrivalsRepo.resetMessages(any(), messagesArgumentCaptor.capture())).thenReturn(Future.successful(true))

        when(arrivalsLockRepo.lock(arrivalIdArgumentCaptor.capture()))
          .thenReturn(Future.successful(true))
          .thenReturn(Future.successful(false))
          .thenReturn(Future.successful(true))

        val successfulUpdates: Seq[Seq[Arrival]] = source
          .via(addJsonToMessagesTransformer.flow)
          .map(_.map(_._1))
          .runWith(TestSink.probe[Seq[Arrival]])
          .request(3)
          .expectNextN(2)

        arrivalIdArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.arrivalId)
        successfulUpdates must contain theSameElementsAs (List(List(arrivals(0)), List(arrivals(2))))
        messagesArgumentCaptor.getAllValues must contain theSameElementsAs List(arrivals(0).messages, arrivals(2).messages)
      }

      "when the arrival locking fails" in new Fixture {
        val arrivals = listOfArrivals(3).sample.value
        val source   = Source(arrivals)

        when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings)
        when(arrivalsRepo.resetMessages(any(), messagesArgumentCaptor.capture())).thenReturn(Future.successful(true))

        when(arrivalsLockRepo.lock(arrivalIdArgumentCaptor.capture()))
          .thenReturn(Future.successful(true))
          .thenReturn(Future.failed(new Exception("locking failed")))
          .thenReturn(Future.successful(true))

        val successfulUpdates: Seq[Seq[Arrival]] = source
          .via(addJsonToMessagesTransformer.flow)
          .map(_.map(_._1))
          .runWith(TestSink.probe[Seq[Arrival]])
          .request(3)
          .expectNextN(2)

        arrivalIdArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.arrivalId)
        successfulUpdates must contain theSameElementsAs (List(List(arrivals(0)), List(arrivals(2))))
        messagesArgumentCaptor.getAllValues must contain theSameElementsAs List(arrivals(0).messages, arrivals(2).messages)
      }

      "when the arrival unlocking cannot be performed" in new Fixture {
        val arrivals = listOfArrivals(3).sample.value
        val source   = Source(arrivals)

        when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings)
        when(arrivalsRepo.resetMessages(any(), messagesArgumentCaptor.capture())).thenReturn(Future.successful(true))

        when(arrivalsLockRepo.unlock(arrivalIdArgumentCaptor.capture()))
          .thenReturn(Future.successful(true))
          .thenReturn(Future.successful(false))
          .thenReturn(Future.successful(true))

        val successfulUpdates: Seq[Seq[Arrival]] = source
          .via(addJsonToMessagesTransformer.flow)
          .map(_.map(_._1))
          .runWith(TestSink.probe[Seq[Arrival]])
          .request(3)
          .expectNextN(3)

        arrivalIdArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.arrivalId)
        successfulUpdates must contain theSameElementsAs arrivals.map(Seq(_))
        messagesArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.messages)
      }

      "when the arrival unlocking fails" in new Fixture {
        val arrivals = listOfArrivals(3).sample.value
        val source   = Source(arrivals)

        when(workerConfig.addJsonToMessagesWorkerSettings).thenReturn(defaultSettings)
        when(arrivalsRepo.resetMessages(any(), messagesArgumentCaptor.capture())).thenReturn(Future.successful(true))

        when(arrivalsLockRepo.unlock(arrivalIdArgumentCaptor.capture()))
          .thenReturn(Future.successful(true))
          .thenReturn(Future.failed(new Exception("unlocking failed")))
          .thenReturn(Future.successful(true))

        val successfulUpdates: Seq[Seq[Arrival]] = source
          .via(addJsonToMessagesTransformer.flow)
          .map(_.map(_._1))
          .runWith(TestSink.probe[Seq[Arrival]])
          .request(3)
          .expectNextN(3)

        arrivalIdArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.arrivalId)
        successfulUpdates must contain theSameElementsAs arrivals.map(Seq(_))
        messagesArgumentCaptor.getAllValues must contain theSameElementsAs arrivals.map(_.messages)
      }

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

    def addJsonToMessagesTransformer = new AddJsonToMessagesTransformer(workerConfig, arrivalsRepo, arrivalsLockRepo, emptyConverter)
  }
}
