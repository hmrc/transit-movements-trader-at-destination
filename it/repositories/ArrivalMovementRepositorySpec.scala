/*
 * Copyright 2022 HM Revenue & Customs
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

package repositories

import akka.Done
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import base._
import cats.data.Chain
import cats.data.Ior
import cats.data.NonEmptyList
import com.kenshoo.play.metrics.Metrics
import config.AppConfig
import controllers.routes
import models.ChannelType.api
import models.ChannelType.web
import models._
import models.response.ResponseArrival
import models.response.ResponseArrivals
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.source
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.BeforeAndAfterEach
import org.scalatest.TestSuiteMixin
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.Configuration
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.test.Helpers.running
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection
import utils.Format

import java.time._
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class ArrivalMovementRepositorySpec
    extends ItSpecBase
    with MongoSuite
    with ScalaFutures
    with TestSuiteMixin
    with MongoDateTimeFormats
    with BeforeAndAfterEach
    with MockitoSugar {

  private val instant                   = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  implicit private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  private val appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .overrides(bind[Clock].toInstance(stubClock))
      .configure(
        "metrics.jvm" -> false
      )

  override def beforeEach(): Unit = {
    database.flatMap(_.drop).futureValue
    super.beforeEach()
  }

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult =>
      throw new TestFailedException(
        (_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
        None,
        implicitly[source.Position]
      )
  }

  def nonEmptyListOfNArrivals(size: Int): Gen[NonEmptyList[Arrival]] =
    Gen
      .listOfN(size, arbitrary[Arrival])
      // Don't generate duplicate IDs
      .map(_.foldLeft((Chain.empty[Arrival], 1)) {
        case ((arrivals, id), arrival) =>
          (arrivals :+ arrival.copy(arrivalId = ArrivalId(id)), id + 1)
      })
      .map {
        case (arrivals, _) =>
          NonEmptyList.fromListUnsafe(arrivals.toList)
      }

  private val eoriNumber: String = arbitrary[String].sample.value
  private val turn: String       = arbitrary[String].sample.value
  private val mrn                = arbitrary[MovementReferenceNumber].sample.value

  "ArrivalMovementRepository" - {

    "started" - {
      "must ensure indexes" in {

        val app = appBuilder.build()
        running(app) {

          val indexes = database
            .flatMap {
              result =>
                result.collection[JSONCollection](ArrivalMovementRepository.collectionName).indexesManager.list()
            }
            .futureValue
            .map {
              index =>
                (index.name.get, index.key)
            }

          indexes must contain theSameElementsAs List(
            ("movement-reference-number-index", Seq(("movementReferenceNumber", IndexType.Ascending))),
            (
              "fetch-all-with-date-filter-index",
              Seq(("channel", IndexType.Ascending), ("eoriNumber", IndexType.Ascending), ("lastUpdated", IndexType.Descending))
            ),
            ("fetch-all-index", Seq(("channel", IndexType.Ascending), ("eoriNumber", IndexType.Ascending))),
            ("channel-index", Seq(("channel", IndexType.Ascending))),
            ("eori-number-index", Seq(("eoriNumber", IndexType.Ascending))),
            ("last-updated-index", Seq(("lastUpdated", IndexType.Ascending))),
            ("_id_", Seq(("_id", IndexType.Ascending)))
          )
        }
      }
    }

    "started -- testing change in TTL" - {

      val indexUnderTest = "last-updated-index";

      class Harness(
        cn: String,
        mongo: ReactiveMongoApi,
        appConfig: AppConfig,
        config: Configuration,
        clock: Clock,
        override val metrics: Metrics
      )(implicit ec: ExecutionContext, m: Materializer)
          extends ArrivalMovementRepository(mongo, appConfig, config, clock, metrics)(ec, m) {
        override lazy val collectionName: String = cn
      }

      def createHarness(app: Application, name: String) =
        new Harness(
          name,
          app.injector.instanceOf[ReactiveMongoApi],
          app.injector.instanceOf[AppConfig],
          app.injector.instanceOf[Configuration],
          app.injector.instanceOf[Clock],
          app.injector.instanceOf[Metrics]
        )(app.materializer.executionContext, app.materializer)

      def runTest(name: String, ttl1: Int, ttl2: Int): Future[List[Index]] = {
        // avoids starting the "real" repository, as index changes occur in the initialiser.
        val builder = appBuilder.overrides(bind[ArrivalMovementRepository].to(mock[ArrivalMovementRepository]))
        val mongo   = builder.injector.instanceOf[ReactiveMongoApi]

        def callHarness(ttl: Int): Harness = {
          val app = builder
            .configure(Configuration("mongodb.timeToLiveInSeconds" -> ttl))
            .build()

          createHarness(app, name)
        }

        // We run the harness once to create the collection,
        // then run it again to simulate re-connecting to it.
        callHarness(ttl1).started
          .flatMap(
            _ => callHarness(ttl2).started
          )
          .flatMap(
            _ => mongo.database
          )
          .flatMap(_.collection[JSONCollection](name).indexesManager.list)
      }

      "must persist the same TTL if required" in {
        whenReady(runTest("ttl-same", 200, 200)) {
          indexes =>
            val filtered: Option[Index] = indexes.filter(_.name.contains(indexUnderTest)).headOption
            filtered match {
              case Some(x) =>
                x.expireAfterSeconds.get mustBe 200
              case None =>
                fail(s"Index $indexUnderTest does not exist or does not have a TTL")
            }
        }
      }

      "must use the new TTL if required" in {
        whenReady(runTest("ttl-different", 200, 300)) {
          indexes =>
            val filtered: Option[Index] = indexes.filter(_.name.contains(indexUnderTest)).headOption
            filtered match {
              case Some(x) =>
                x.expireAfterSeconds.get mustBe 300
              case None =>
                fail(s"Index $indexUnderTest does not exist or does not have a TTL")
            }
        }
      }

    }

    "insert" - {
      "must persist ArrivalMovement within mongoDB" in {

        val arrival = arbitrary[Arrival].sample.value

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          val selector = Json.obj("eoriNumber" -> arrival.eoriNumber)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](ArrivalMovementRepository.collectionName).find(selector, None).one[Arrival]
          }.futureValue

          result.value mustBe arrival.copy(lastUpdated = result.value.lastUpdated)
        }
      }
    }

    "getMaxArrivalId" - {
      "must return the highest arrival id in the database" in {
        database.flatMap(_.drop()).futureValue
        val app = appBuilder.configure("feature-flags.testOnly.enabled" -> true).build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrivals = List.tabulate(5)(
            index => arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(index + 1))
          )

          repository.bulkInsert(arrivals).futureValue

          repository.getMaxArrivalId.futureValue.value mustBe ArrivalId(5)
        }
      }
    }

    "addResponseMessage" - {
      "must add a message, update the status of a document and update the timestamp" in {

        val arrival = arbitrary[Arrival].sample.value

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)

        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            arrival.nextMessageCorrelationId
          )

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val addMessageResult = repository.addResponseMessage(arrival.arrivalId, goodsReleasedMessage).futureValue

          val selector = Json.obj("_id" -> arrival.arrivalId)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](ArrivalMovementRepository.collectionName).find(selector, None).one[Arrival]
          }.futureValue

          val updatedArrival = result.value

          addMessageResult mustBe a[Success[_]]
          updatedArrival.nextMessageCorrelationId - arrival.nextMessageCorrelationId mustBe 0
          updatedArrival.updated mustEqual goodsReleasedMessage.received.get
          updatedArrival.lastUpdated mustEqual goodsReleasedMessage.received.get
          updatedArrival.messages.size - arrival.messages.size mustEqual 1
          updatedArrival.messages.last mustEqual goodsReleasedMessage
        }
      }

      "must fail if the arrival cannot be found" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)
        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            messageCorrelationId = 1
          )

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addResponseMessage(ArrivalId(2), goodsReleasedMessage).futureValue

          result mustBe a[Failure[_]]
        }
      }
    }

    "addNewMessage" - {
      "must add a message, update the timestamp and increment nextCorrelationId" in {

        val arrival = arbitrary[Arrival].sample.value

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)

        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            arrival.nextMessageCorrelationId
          )

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          repository.addNewMessage(arrival.arrivalId, goodsReleasedMessage).futureValue.success

          val selector = Json.obj("_id" -> arrival.arrivalId)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](ArrivalMovementRepository.collectionName).find(selector, None).one[Arrival]
          }.futureValue

          val updatedArrival = result.value

          updatedArrival.nextMessageCorrelationId - arrival.nextMessageCorrelationId mustBe 1
          updatedArrival.updated mustEqual goodsReleasedMessage.received.get
          updatedArrival.lastUpdated mustEqual goodsReleasedMessage.received.get
          updatedArrival.messages.size - arrival.messages.size mustEqual 1
          updatedArrival.messages.last mustEqual goodsReleasedMessage
        }
      }

      "must fail if the arrival cannot be found" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)
        val messageBody =
          <CC025A>
            <DatOfPreMES9>
              {Format.dateFormatted(dateOfPrep)}
            </DatOfPreMES9>
            <TimOfPreMES10>
              {Format.timeFormatted(timeOfPrep)}
            </TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            messageCorrelationId = 1
          )

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addNewMessage(ArrivalId(2), goodsReleasedMessage).futureValue

          result mustBe a[Failure[_]]
        }
      }
    }

    "get(arrivalId: ArrivalId)" - {
      "must get an departure when it exists and has the right channel type" in {
        database.flatMap(_.drop()).futureValue

        val app = appBuilder.build()
        running(app) {
          val arrival = arbitrary[Arrival].sample.value

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.get(arrival.arrivalId)

          whenReady(result) {
            r =>
              r.value mustEqual arrival
          }
        }

      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val app = appBuilder.build()
        running(app) {
          val arrival = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2))

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.get(ArrivalId(1))

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }
    }

    "get(arrivalId: ArrivalId, channelFilter: ChannelType)" - {
      "must get an arrival when it exists and has the right channel type" in {

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value

          repository.insert(arrival).futureValue
          val result = repository.get(arrival.arrivalId, arrival.channel).futureValue

          result.value mustEqual arrival.copy(lastUpdated = result.value.lastUpdated)
        }
      }

      "must return None when an arrival does not exist" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

          repository.insert(arrival).futureValue
          val result = repository.get(ArrivalId(2), arrival.channel).futureValue

          result must not be defined
        }
      }

      "must return None when an arrival does exist but with the wrong channel type" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (channel = api)

          repository.insert(arrival).futureValue
          val result = repository.get(arrival.arrivalId, web).futureValue

          result must not be defined
        }
      }
    }

    "getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType])" - {

      val mType = MessageType.UnloadingPermission

      val node = NodeSeq.fromSeq(Seq(<CC043A>m</CC043A>))

      val message = MovementMessageWithStatus(
        MessageId(1),
        LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS),
        Some(LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS)),
        MessageType.UnloadingPermission,
        node,
        MessageStatus.SubmissionSucceeded,
        1
      )

      val otherMessage = MovementMessageWithStatus(
        MessageId(2),
        LocalDateTime.now(),
        Some(LocalDateTime.now()),
        MessageType.GoodsReleased,
        node,
        MessageStatus.SubmissionSucceeded,
        2
      )

      "must get the appropriate messages when they exist and has the right channel type" in {

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (messages = NonEmptyList[MovementMessage](message, List.empty))

          repository.insert(arrival).futureValue

          // We copy the message node because the returned node isn't equal, even though it's
          // identical for our purposes. As it's not what we are really testing, we just copy the
          // original message across so it doesn't fail the equality check
          val result = repository
            .getMessagesOfType(arrival.arrivalId, arrival.channel, List(mType))
            .map(
              opt =>
                opt.map(
                  ar =>
                    ar.messages
                      .asInstanceOf[List[MovementMessageWithStatus]]
                      .map(
                        x => x copy (message = node)
                      )
                )
            )
            .futureValue

          result mustBe defined
          result.get must contain theSameElementsAs List(message)
        }
      }

      "must only return the appropriate messages when an arrival is matched" in {

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (messages = NonEmptyList[MovementMessage](message, List(otherMessage)))

          repository.insert(arrival).futureValue

          // As in the previous test.
          val result = repository
            .getMessagesOfType(arrival.arrivalId, arrival.channel, List(mType))
            .map(
              opt =>
                opt.map(
                  ar =>
                    ar.messages
                      .asInstanceOf[List[MovementMessageWithStatus]]
                      .map(
                        x => x copy (message = node)
                      )
                )
            )
            .futureValue

          result mustBe defined
          result.get must contain theSameElementsAs List(message)
        }
      }

      "must return an empty list when an arrival exists but no messages match" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), messages = NonEmptyList[MovementMessage](otherMessage, List.empty))

          repository.insert(arrival).futureValue
          val result = repository.getMessagesOfType(ArrivalId(1), arrival.channel, List(mType)).futureValue

          result mustBe defined
          result.get.messages mustEqual List()
        }
      }

      "must return None when an arrival does not exist" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

          repository.insert(arrival).futureValue
          val result = repository.getMessagesOfType(ArrivalId(2), arrival.channel, List(mType)).futureValue

          result must not be defined
        }
      }

      "must return an empty list when an arrival exists but without any of the message type" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

          repository.insert(arrival).futureValue
          val result = repository.getMessagesOfType(ArrivalId(2), arrival.channel, List(mType)).futureValue

          result mustBe empty
        }
      }

      "must return None when an arrival does exist but with the wrong channel type" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (channel = api)

          repository.insert(arrival).futureValue
          val result = repository.getMessagesOfType(arrival.arrivalId, web, List(mType)).futureValue

          result must not be defined
        }
      }

    }

    "getWithoutMessages(arrivalId: ArrivalId)" - {
      "must get an arrival when it exists" in {

        val app = appBuilder.build()
        running(app) {
          val service = app.injector.instanceOf[ArrivalMovementRepository]
          database.flatMap(_.drop()).futureValue

          val arrival                = arbitrary[Arrival].sample.value
          val arrivalWithoutMessages = ArrivalWithoutMessages.fromArrival(arrival)
          service.insert(arrival).futureValue
          val result = service.getWithoutMessages(arrival.arrivalId)

          whenReady(result) {
            r =>
              r.value mustEqual arrivalWithoutMessages
          }
        }
      }

      "must return None when an arrival does not exist" in {
        val app = appBuilder.build()
        running(app) {
          val service = app.injector.instanceOf[ArrivalMovementRepository]

          database.flatMap(_.drop()).futureValue

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

          service.insert(arrival).futureValue
          val result = service.getWithoutMessages(ArrivalId(2), web)

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }

      "must return None when a arrival exists, but with a different channel type" in {
        val app = appBuilder.build()
        running(app) {
          val service = app.injector.instanceOf[ArrivalMovementRepository]

          database.flatMap(_.drop()).futureValue

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), api)

          service.insert(arrival).futureValue
          val result = service.get(ArrivalId(1), web)

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }
    }

    "getWithoutMessages(arrivalId: ArrivalId, channelFilter: ChannelType)" - {
      "must get an arrival when it exists and has the right channel type" in {
        val app = appBuilder.build()
        running(app) {
          val service = app.injector.instanceOf[ArrivalMovementRepository]
          database.flatMap(_.drop()).futureValue

          val arrival                = arbitrary[Arrival].sample.value.copy(channel = api)
          val arrivalWithoutMessages = ArrivalWithoutMessages.fromArrival(arrival)
          service.insert(arrival).futureValue
          val result = service.getWithoutMessages(arrival.arrivalId, arrival.channel)

          whenReady(result) {
            r =>
              r.value mustEqual arrivalWithoutMessages
          }
        }
      }

      "must return None when an arrival does not exist" in {
        val app = appBuilder.build()
        running(app) {
          val service = app.injector.instanceOf[ArrivalMovementRepository]
          database.flatMap(_.drop()).futureValue

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), channel = api)

          service.insert(arrival).futureValue
          val result = service.getWithoutMessages(ArrivalId(2), web)

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }

      "must return None when an arrival exists, but with a different channel type" in {
        val app = appBuilder.build()
        running(app) {
          val service = app.injector.instanceOf[ArrivalMovementRepository]
          database.flatMap(_.drop()).futureValue

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), api)

          service.insert(arrival).futureValue
          val result = service.get(ArrivalId(1), web)

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }
    }

    "fetchAllArrivals" - {
      "must return Arrival Movements that match an eoriNumber and channel type" in {

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = eoriNumber, channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(arrivalId = ArrivalId(1), channel = api)
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, arrivalId = ArrivalId(2), channel = web)

        val app = appBuilder.build()
        running(app) {

          val repository: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val expectedApiFetchAll1 = ResponseArrivals(
            Seq(
              ResponseArrival(
                arrivalMovement1.arrivalId,
                routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
                routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
                arrivalMovement1.movementReferenceNumber,
                arrivalMovement1.status,
                arrivalMovement1.created,
                arrivalMovement1.lastUpdated
              )
            ),
            1,
            1,
            1
          )

          val expectedApiFetchAll3 = ResponseArrivals(
            Seq(
              ResponseArrival(
                arrivalMovement3.arrivalId,
                routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
                routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
                arrivalMovement3.movementReferenceNumber,
                arrivalMovement3.status,
                arrivalMovement3.created,
                arrivalMovement3.lastUpdated
              )
            ),
            1,
            1,
            1
          )

          repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), api, None).futureValue mustBe expectedApiFetchAll1
          repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None).futureValue mustBe expectedApiFetchAll3
        }
      }

      "must return Arrival Movements with eoriNumber that match legacy TURN and channel type" in {

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = turn, channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != turn).sample.value.copy(arrivalId = ArrivalId(1), channel = api)
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2), eoriNumber = turn, channel = web)

        val app = appBuilder.build()
        running(app) {

          val repository: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val expectedApiFetchAll1 = ResponseArrivals(
            Seq(
              ResponseArrival(
                arrivalMovement1.arrivalId,
                routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
                routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
                arrivalMovement1.movementReferenceNumber,
                arrivalMovement1.status,
                arrivalMovement1.created,
                arrivalMovement1.lastUpdated
              )
            ),
            1,
            1,
            1
          )

          val expectedApiFetchAll3 = ResponseArrivals(
            Seq(
              ResponseArrival(
                arrivalMovement3.arrivalId,
                routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
                routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
                arrivalMovement3.movementReferenceNumber,
                arrivalMovement3.status,
                arrivalMovement3.created,
                arrivalMovement3.lastUpdated
              )
            ),
            1,
            1,
            1
          )

          repository.fetchAllArrivals(Ior.left(TURN(turn)), api, None).futureValue mustBe expectedApiFetchAll1
          repository.fetchAllArrivals(Ior.left(TURN(turn)), web, None).futureValue mustBe expectedApiFetchAll3
        }
      }

      "must return Arrival Movements with eoriNumber that match either EORI or legacy TURN and channel type" in {

        val ids: Set[String] = Set(eoriNumber, turn)

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = eoriNumber, channel = api)
        val arrivalMovement2 = arbitrary[Arrival]
          .suchThat(
            arrival => !ids.contains(arrival.eoriNumber)
          )
          .sample
          .value
          .copy(arrivalId = ArrivalId(1), channel = api)
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2), eoriNumber = turn, channel = web)

        val app = appBuilder.build()
        running(app) {

          val repository: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val expectedApiFetchAll1 = ResponseArrivals(
            Seq(
              ResponseArrival(
                arrivalMovement1.arrivalId,
                routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
                routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
                arrivalMovement1.movementReferenceNumber,
                arrivalMovement1.status,
                arrivalMovement1.created,
                arrivalMovement1.lastUpdated
              )
            ),
            1,
            1,
            1
          )

          val expectedApiFetchAll3 = ResponseArrivals(
            Seq(
              ResponseArrival(
                arrivalMovement3.arrivalId,
                routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
                routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
                arrivalMovement3.movementReferenceNumber,
                arrivalMovement3.status,
                arrivalMovement3.created,
                arrivalMovement3.lastUpdated
              )
            ),
            1,
            1,
            1
          )

          repository.fetchAllArrivals(Ior.both(TURN(turn), EORINumber(eoriNumber)), api, None).futureValue mustBe expectedApiFetchAll1
          repository.fetchAllArrivals(Ior.both(TURN(turn), EORINumber(eoriNumber)), web, None).futureValue mustBe expectedApiFetchAll3
        }
      }

      "must return an empty sequence when there are no movements with the same eori" in {

        val arrivalMovement1 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)

        val app = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val result = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), api, None).futureValue

          result mustBe ResponseArrivals(Seq.empty, 0, 0, 0)
        }
      }

      "must filter results by lastUpdated when updatedSince parameter is provided" in {

        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(0),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31)
          )
        val arrivalMovement2 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(1),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 35, 32)
          )
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(2),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 12, 30, 21)
          )
        val arrivalMovement4 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(3),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 10, 15, 16)
          )

        val app = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          // We must use the web channel for this test as the API max rows returned in integration test config is 2
          val dateTime = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
          val actual   = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, Some(dateTime)).futureValue
          val expected = ResponseArrivals(Seq(arrivalMovement3, arrivalMovement4, arrivalMovement2).map(ResponseArrival.build), 3, 4, 3)

          actual mustEqual expected
        }
      }

      "must filter results by mrn when mrn search parameter provided matches" in {
        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = eoriNumber, channel = web, movementReferenceNumber = mrn)
        val arrivalMovement2 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(1), eoriNumber = eoriNumber, channel = web, movementReferenceNumber = mrn)
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2), eoriNumber = eoriNumber, channel = web, movementReferenceNumber = mrn)
        val arrivalMovement4 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(3), eoriNumber = eoriNumber, channel = web, movementReferenceNumber = mrn)

        val app = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)

          val expectedAllMovements = allMovements.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse
          val jsonArr              = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val actual   = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value)).futureValue
          val expected = ResponseArrivals(expectedAllMovements, 4, 4, 4)

          actual mustEqual expected
        }
      }

      "must filter results by mrn when substring of a mrn search parameter provided matches" in {
        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = eoriNumber, channel = web, movementReferenceNumber = mrn)
        val arrivalMovement2 =
          arbitrary[Arrival].suchThat(_.movementReferenceNumber != mrn).sample.value.copy(arrivalId = ArrivalId(1), eoriNumber = eoriNumber, channel = web)
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2), eoriNumber = eoriNumber, channel = web, movementReferenceNumber = mrn)
        val arrivalMovement4 =
          arbitrary[Arrival].suchThat(_.movementReferenceNumber != mrn).sample.value.copy(arrivalId = ArrivalId(3), eoriNumber = eoriNumber, channel = web)

        val app = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements        = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)
          val allMovementsMatched = Seq(arrivalMovement1, arrivalMovement3)

          val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse
          val jsonArr              = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val actual = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value.substring(3, 9))).futureValue

          val expected = ResponseArrivals(expectedAllMovements, 2, 4, 2)

          actual mustEqual expected
        }
      }

      "must filter results by mrn when mrn search parameter provided matches return match count" in {
        val arrivals = nonEmptyListOfNArrivals(10)
          .map(_.toList)
          .sample
          .value
          .map(_.copy(eoriNumber = eoriNumber, movementReferenceNumber = mrn, channel = web))

        val arrivalMovement1 =
          arbitrary[Arrival]
            .retryUntil(
              x => x.movementReferenceNumber != mrn && !arrivals.map(_.arrivalId).contains(x.arrivalId)
            )
            .sample
            .value
            .copy(eoriNumber = eoriNumber, channel = web)

        val allArrivals = arrivalMovement1 :: arrivals

        val aJsonArr = allArrivals.map(Json.toJsObject(_))
        val app      = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovementsMatched = arrivals

          val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(aJsonArr)
          }.futureValue

          val actual = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value.substring(4, 9))).futureValue

          val expected = ResponseArrivals(expectedAllMovements, arrivals.size, allArrivals.size, arrivals.size)

          actual mustEqual expected
        }
      }

      "must filter results by mrn when mrn search parameter  with case insensitive provided matches return match count" in {
        val arrivals = nonEmptyListOfNArrivals(20)
          .map(_.toList)
          .sample
          .value
          .map(_.copy(eoriNumber = eoriNumber, movementReferenceNumber = mrn, channel = web))

        val arrivalMovement1 =
          arbitrary[Arrival]
            .retryUntil(
              x => x.movementReferenceNumber != mrn && !arrivals.map(_.arrivalId).contains(x.arrivalId)
            )
            .sample
            .value
            .copy(eoriNumber = eoriNumber, channel = web)

        val allArrivals = arrivalMovement1 :: arrivals

        val aJsonArr = allArrivals.map(Json.toJsObject(_))
        val app      = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovementsMatched = arrivals

          val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(aJsonArr)
          }.futureValue

          val actual = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value.substring(4, 9).toLowerCase())).futureValue

          val expected = ResponseArrivals(expectedAllMovements, arrivals.size, allArrivals.size, arrivals.size)

          actual mustEqual expected
        }
      }

      "must fetch all results based on pageSize 5 for page number 2" in {
        val arrivals = nonEmptyListOfNArrivals(20)
          .map(_.toList)
          .sample
          .value
          .map(_.copy(eoriNumber = eoriNumber, movementReferenceNumber = mrn, channel = web))

        val pageSize    = 5
        val page        = 2
        val allArrivals = arrivals

        val aJsonArr = allArrivals.map(Json.toJsObject(_))
        val app      = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovementsMatched = arrivals

          val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse.slice(5, 10)

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(aJsonArr)
          }.futureValue

          val actual = service.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, None, Some(pageSize), Some(page)).futureValue

          val expected = ResponseArrivals(expectedAllMovements, pageSize, allArrivals.size, allArrivals.size)

          actual mustEqual expected
        }
      }
    }

    "arrivalsWithoutJsonMessagesSource" - {
      "must return arrivals with any messages that don't have a JSON representation, or whose JSON representation is an empty JSON object" in {
        implicit val actorSystem: ActorSystem = ActorSystem()

        val arrival1 = arbitrary[Arrival].map(_.copy(ArrivalId(1))).sample.value
        val arrival2 = arbitrary[Arrival].map(_.copy(ArrivalId(2))).sample.value
        val arrival3 = arbitrary[Arrival].map(_.copy(ArrivalId(3))).sample.value
        val arrival4 = arbitrary[Arrival].map(_.copy(ArrivalId(4))).sample.value

        val messageWithJson      = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj("foo" -> "bar"))
        val messageWithoutJson   = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] - "messageJson"
        val messageWithEmptyJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj())

        val arrivalWithJson      = Json.toJson(arrival1).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson))
        val arrivalWithoutJson   = Json.toJson(arrival2).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithoutJson))
        val arrivalWithSomeJson  = Json.toJson(arrival3).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson, messageWithoutJson))
        val arrivalWithEmptyJson = Json.toJson(arrival4).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithEmptyJson))

        val app = appBuilder.build()
        running(app) {

          val repo = app.injector.instanceOf[ArrivalMovementRepository]

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName)
                .insert(false)
                .many(Seq(arrivalWithJson, arrivalWithoutJson, arrivalWithSomeJson, arrivalWithEmptyJson))
          }.futureValue

          val source: Source[Arrival, Future[Done]] = repo.arrivalsWithoutJsonMessagesSource(3).futureValue

          source
            .map(_.arrivalId)
            .runWith(TestSink.probe[ArrivalId])
            .request(3)
            .expectNextN(List(arrival2.arrivalId, arrival3.arrivalId, arrival4.arrivalId))
        }
      }

      "must return a stream that only returns the requested number of results" in {
        implicit val actorSystem: ActorSystem = ActorSystem()

        val arrival1 = arbitrary[Arrival].map(_.copy(ArrivalId(1))).sample.value
        val arrival2 = arbitrary[Arrival].map(_.copy(ArrivalId(2))).sample.value
        val arrival3 = arbitrary[Arrival].map(_.copy(ArrivalId(3))).sample.value
        val arrival4 = arbitrary[Arrival].map(_.copy(ArrivalId(4))).sample.value

        val messageWithJson      = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj("foo" -> "bar"))
        val messageWithoutJson   = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] - "messageJson"
        val messageWithEmptyJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj())

        val arrivalWithJson      = Json.toJson(arrival1).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson))
        val arrivalWithoutJson   = Json.toJson(arrival2).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithoutJson))
        val arrivalWithSomeJson  = Json.toJson(arrival3).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson, messageWithoutJson))
        val arrivalWithEmptyJson = Json.toJson(arrival4).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithEmptyJson))

        val app = appBuilder.build()
        running(app) {

          val repo = app.injector.instanceOf[ArrivalMovementRepository]

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName)
                .insert(false)
                .many(Seq(arrivalWithJson, arrivalWithoutJson, arrivalWithSomeJson, arrivalWithEmptyJson))
          }.futureValue

          val source: Source[Arrival, Future[Done]] = repo.arrivalsWithoutJsonMessagesSource(1).futureValue

          source
            .map(_.arrivalId)
            .runWith(TestSink.probe[ArrivalId])
            .request(2)
            .expectNext(arrival2.arrivalId)
            .expectComplete()
        }
      }
    }

    ".arrivalsWithoutJsonMessages" - {

      "must return arrivals with any messages that don't have a JSON representation, or whose JSON representation is an empty JSON object" in {

        val arrival1 = arbitrary[Arrival].map(_.copy(ArrivalId(1))).sample.value
        val arrival2 = arbitrary[Arrival].map(_.copy(ArrivalId(2))).sample.value
        val arrival3 = arbitrary[Arrival].map(_.copy(ArrivalId(3))).sample.value
        val arrival4 = arbitrary[Arrival].map(_.copy(ArrivalId(4))).sample.value

        val messageWithJson      = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj("foo" -> "bar"))
        val messageWithoutJson   = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] - "messageJson"
        val messageWithEmptyJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj())

        val arrivalWithJson      = Json.toJson(arrival1).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson))
        val arrivalWithoutJson   = Json.toJson(arrival2).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithoutJson))
        val arrivalWithSomeJson  = Json.toJson(arrival3).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson, messageWithoutJson))
        val arrivalWithEmptyJson = Json.toJson(arrival4).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithEmptyJson))

        val app = appBuilder.build()
        running(app) {

          val repo = app.injector.instanceOf[ArrivalMovementRepository]

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName)
                .insert(false)
                .many(Seq(arrivalWithJson, arrivalWithoutJson, arrivalWithSomeJson, arrivalWithEmptyJson))
          }.futureValue

          val result = repo.arrivalsWithoutJsonMessages(100).futureValue

          result.size mustEqual 3
          result.exists(
            arrival => arrival.arrivalId == arrival1.arrivalId
          ) mustEqual false
          result.exists(
            arrival => arrival.arrivalId == arrival2.arrivalId
          ) mustEqual true
          result.exists(
            arrival => arrival.arrivalId == arrival3.arrivalId
          ) mustEqual true
          result.exists(
            arrival => arrival.arrivalId == arrival4.arrivalId
          ) mustEqual true
        }
      }
    }

    ".resetMessages" - {

      "must replace the messages of an arrival with the newly-supplied ones" in {

        val arrival     = arbitrary[Arrival].sample.value
        val message1    = arbitrary[MovementMessageWithStatus].sample.value
        val message2    = arbitrary[MovementMessageWithStatus].sample.value
        val newMessages = NonEmptyList(message1, List(message2))

        val app = appBuilder.build()
        running(app) {
          val repo = app.injector.instanceOf[ArrivalMovementRepository]

          repo.insert(arrival).futureValue

          val resetResult    = repo.resetMessages(arrival.arrivalId, newMessages).futureValue
          val updatedArrival = repo.get(arrival.arrivalId).futureValue

          resetResult mustEqual true
          updatedArrival.value mustEqual arrival.copy(messages = newMessages)
        }
      }

      "Must return max 2 arrivals when the API maxRowsReturned = 2" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().build()
        val eoriNumber: String = arbitrary[String].sample.value
        val appConfig          = app.injector.instanceOf[AppConfig]

        val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)
        val id1         = ArrivalId(1)
        val id2         = ArrivalId(2)
        val id3         = ArrivalId(3)
        val movement1   = arbitrary[Arrival].sample.value.copy(arrivalId = id1, eoriNumber = eoriNumber, channel = api, lastUpdated = lastUpdated.withSecond(10))
        val movement2   = arbitrary[Arrival].sample.value.copy(arrivalId = id2, eoriNumber = eoriNumber, channel = api, lastUpdated = lastUpdated.withSecond(20))
        val movement3   = arbitrary[Arrival].sample.value.copy(arrivalId = id3, eoriNumber = eoriNumber, channel = api, lastUpdated = lastUpdated.withSecond(30))

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]
          repository.insert(movement1).futureValue
          repository.insert(movement2).futureValue
          repository.insert(movement3).futureValue

          val maxRows = appConfig.maxRowsReturned(api)
          maxRows mustBe 2

          val movements = repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), api, updatedSince = None).futureValue

          movements.arrivals.size mustBe maxRows
          movements.retrievedArrivals mustBe maxRows

          val ids = movements.arrivals.map(
            m => m.arrivalId.index
          )

          ids mustBe Seq(movement3.arrivalId.index, movement2.arrivalId.index)

        }
      }

      "Must return max 1 arrivals when the WEB maxRowsReturned = 2" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().build()
        val eoriNumber: String = arbitrary[String].sample.value
        val appConfig          = app.injector.instanceOf[AppConfig]

        val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)
        val id1         = ArrivalId(11)
        val id2         = ArrivalId(12)
        val id3         = ArrivalId(13)
        val movement1   = arbitrary[Arrival].sample.value.copy(arrivalId = id1, eoriNumber = eoriNumber, channel = web, lastUpdated = lastUpdated.withSecond(1))
        val movement2   = arbitrary[Arrival].sample.value.copy(arrivalId = id2, eoriNumber = eoriNumber, channel = web, lastUpdated = lastUpdated.withSecond(2))
        val movement3   = arbitrary[Arrival].sample.value.copy(arrivalId = id3, eoriNumber = eoriNumber, channel = web, lastUpdated = lastUpdated.withSecond(3))

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]
          repository.insert(movement1).futureValue
          repository.insert(movement2).futureValue
          repository.insert(movement3).futureValue

          val maxRows = appConfig.maxRowsReturned(web)
          maxRows mustBe 100

          val movements = repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, updatedSince = None).futureValue

          movements.arrivals.size mustBe 3
          movements.retrievedArrivals mustBe 3

          val ids = movements.arrivals.map(
            m => m.arrivalId.index
          )

          ids mustBe Seq(movement3.arrivalId.index, movement2.arrivalId.index, movement1.arrivalId.index)

        }
      }
    }

    "getMessage" - {
      "must return Some(message) if arrival and message exists" in {
        database.flatMap(_.drop()).futureValue

        val app = appBuilder.build()

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val message  = arbitrary[models.MovementMessageWithStatus].sample.value.copy(messageId = MessageId(1))
          val messages = new NonEmptyList(message, Nil)
          val arrival  = arbitrary[Arrival].sample.value.copy(channel = api, messages = messages)

          repository.insert(arrival).futureValue
          val result = repository.getMessage(arrival.arrivalId, arrival.channel, MessageId(1))

          whenReady(result) {
            r =>
              r.isDefined mustBe true
              r.value mustEqual message
          }
        }
      }

      "must return None if departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val app = appBuilder.build()

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val result = repository.getMessage(ArrivalId(1), api, MessageId(1))

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }

      "must return None if message does not exist" in {
        database.flatMap(_.drop()).futureValue
        val app = appBuilder.build()

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val message  = arbitrary[models.MovementMessageWithStatus].sample.value.copy(messageId = MessageId(1))
          val messages = new NonEmptyList(message, Nil)
          val arrival  = arbitrary[Arrival].sample.value.copy(channel = api, messages = messages)

          repository.insert(arrival).futureValue
          val result = repository.getMessage(arrival.arrivalId, arrival.channel, MessageId(5))

          whenReady(result) {
            r =>
              r.isDefined mustBe false
          }
        }
      }
    }
  }
}
