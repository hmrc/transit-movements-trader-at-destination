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

package repositories
import akka.Done
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import base._
import cats.data.NonEmptyList
import controllers.routes
import models.ArrivalStatus.{ArrivalSubmitted, GoodsReleased, Initialized, UnloadingRemarksSubmitted}
import models.ChannelType.{api, web}
import models.MessageStatus.{SubmissionPending, SubmissionSucceeded}
import models.{Arrival, ArrivalId, ArrivalIdSelector, ArrivalStatus, ArrivalStatusUpdate, Box, BoxId, MessageId, MessageType, MongoDateTimeFormats, MovementMessageWithStatus, MovementMessageWithoutStatus, MovementReferenceNumber}
import models.response.ResponseArrival
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.source
import org.scalatest.{BeforeAndAfterEach, TestSuiteMixin}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers.running
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.Format
import config.{AppConfig, Constants}
import java.time._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

class ArrivalMovementRepositorySpec
    extends ItSpecBase
    with MongoSuite
    with ScalaFutures
    with TestSuiteMixin
    with MongoDateTimeFormats
    with BeforeAndAfterEach {

  private val instant = Instant.now
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
      throw new TestFailedException((_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
                                    None,
                                    implicitly[source.Position])
  }

  private val eoriNumber: String = arbitrary[String].sample.value

  "ArrivalMovementRepository" - {
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

    "updateArrival" - {
      "must update the arrival and return a Success Unit when successful" in {
        val app = appBuilder.build()
        running(app) {

          val arrivalStatus = ArrivalStatusUpdate(Initialized)
          val arrival = arrivalWithOneMessage.sample.value.copy(status = GoodsReleased)
          val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)
          val selector = ArrivalIdSelector(arrival.arrivalId)

          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          repository.updateArrival(selector, arrivalStatus).futureValue

          val updatedArrival = repository.get(arrival.arrivalId, arrival.channel).futureValue.value

          updatedArrival.status mustEqual arrivalStatus.arrivalStatus
          updatedArrival.lastUpdated.withSecond(0).withNano(0) mustEqual lastUpdated
        }
      }

      "must return a Failure if the selector does not match any documents" in {
        val app = appBuilder.build()
        running(app) {

          val arrivalStatus = ArrivalStatusUpdate(Initialized)
          val arrival = arrivalWithOneMessage.sample.value copy(arrivalId = ArrivalId(1), status = UnloadingRemarksSubmitted)
          val selector = ArrivalIdSelector(ArrivalId(2))

          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          val result = repository.updateArrival(selector, arrivalStatus).futureValue

          val updatedArrival = repository.get(arrival.arrivalId, arrival.channel).futureValue.value

          result mustBe a[Failure[_]]
          updatedArrival.status must not be (arrivalStatus.arrivalStatus)
        }
      }
    }

    "setArrivalStateAndMessageState" - {
      "must update the status of the arrival and the message in an arrival" in {

        val arrival = arrivalWithOneMessage.sample.value.copy(status = ArrivalStatus.Initialized)
        val messageId = MessageId.fromIndex(0)

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          repository.setArrivalStateAndMessageState(arrival.arrivalId, messageId, ArrivalSubmitted, SubmissionSucceeded).futureValue

          val updatedArrival = repository.get(arrival.arrivalId, arrival.channel).futureValue

          updatedArrival.value.status mustEqual ArrivalSubmitted

          typeMatchOnTestValue(updatedArrival.value.messages.head) {
            result: MovementMessageWithStatus =>
              result.status mustEqual SubmissionSucceeded
          }
        }
      }

      "must fail if the arrival cannot be found" in {

        val arrival   = arrivalWithOneMessage.sample.value.copy(arrivalId = ArrivalId(1), status = Initialized)
        val messageId = MessageId.fromIndex(0)

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]
          repository.insert(arrival).futureValue

          val setResult = repository.setArrivalStateAndMessageState(ArrivalId(2), messageId, ArrivalSubmitted, SubmissionSucceeded)
          setResult.futureValue must not be (defined)

          val result = repository.get(arrival.arrivalId, arrival.channel).futureValue.value
          result.status mustEqual Initialized
          typeMatchOnTestValue(result.messages.head) {
            result: MovementMessageWithStatus =>
              result.status mustEqual SubmissionPending
          }
        }
      }
    }

    "addResponseMessage" - {
      "must add a message, update the status of a document and update the timestamp" in {

        val arrival = arbitrary[Arrival].sample.value

        val dateOfPrep  = LocalDate.now(stubClock)
        val timeOfPrep  = LocalTime.now(stubClock).withHour(1).withMinute(1)
        val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)

        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, messageBody, arrival.nextMessageCorrelationId)
        val newState = ArrivalStatus.GoodsReleased

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val addMessageResult = repository.addResponseMessage(arrival.arrivalId, goodsReleasedMessage, newState).futureValue

          val selector = Json.obj("_id" -> arrival.arrivalId)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](ArrivalMovementRepository.collectionName).find(selector, None).one[Arrival]
          }.futureValue

          val updatedArrival = result.value

          addMessageResult mustBe a[Success[_]]
          updatedArrival.nextMessageCorrelationId - arrival.nextMessageCorrelationId mustBe 0
          updatedArrival.updated mustEqual goodsReleasedMessage.dateTime
          updatedArrival.lastUpdated.withSecond(0).withNano(0) mustEqual lastUpdated
          updatedArrival.status mustEqual newState
          updatedArrival.messages.size - arrival.messages.size mustEqual 1
          updatedArrival.messages.last mustEqual goodsReleasedMessage
        }
      }

      "must fail if the arrival cannot be found" in {

        val arrival = arbitrary[Arrival].sample.value copy (status = ArrivalStatus.ArrivalSubmitted, arrivalId = ArrivalId(1))

        val dateOfPrep  = LocalDate.now(stubClock)
        val timeOfPrep  = LocalTime.now(stubClock).withHour(1).withMinute(1)
        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, messageBody, messageCorrelationId = 1)
        val newState = ArrivalStatus.GoodsReleased

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addResponseMessage(ArrivalId(2), goodsReleasedMessage, newState).futureValue

          result mustBe a[Failure[_]]
        }
      }
    }

    "addNewMessage" - {
      "must add a message, update the timestamp and increment nextCorrelationId" in {

        val arrival = arbitrary[Arrival].sample.value.copy(status = ArrivalStatus.ArrivalSubmitted)

        val dateOfPrep  = LocalDate.now(stubClock)
        val timeOfPrep  = LocalTime.now(stubClock).withHour(1).withMinute(1)
        val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)
        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, messageBody, arrival.nextMessageCorrelationId)

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
          updatedArrival.updated mustEqual goodsReleasedMessage.dateTime
          updatedArrival.lastUpdated.withSecond(0).withNano(0) mustEqual lastUpdated
          updatedArrival.status mustEqual arrival.status
          updatedArrival.messages.size - arrival.messages.size mustEqual 1
          updatedArrival.messages.last mustEqual goodsReleasedMessage
        }
      }

      "must fail if the arrival cannot be found" in {

        val arrival = arbitrary[Arrival].sample.value copy(status = ArrivalStatus.ArrivalSubmitted, arrivalId = ArrivalId(1))

        val dateOfPrep  = LocalDate.now(stubClock)
        val timeOfPrep  = LocalTime.now(stubClock).withHour(1).withMinute(1)
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
          MovementMessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, messageBody, messageCorrelationId = 1)

        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addNewMessage(ArrivalId(2), goodsReleasedMessage).futureValue

          result mustBe a[Failure[_]]
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

    "get(eoriNumber: String, mrn: MovementReferenceNumber, channelFilter: ChannelType)" - {
      "must get an arrival if one exists with a matching eoriNumber, channelType and mrn" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value
          val eori = "eori"
          val arrival = arbitrary[Arrival].sample.value copy(eoriNumber = eori, movementReferenceNumber = movementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, arrival.channel).futureValue

          result.value mustEqual arrival.copy(lastUpdated = result.value.lastUpdated)
        }
      }

      "must return a None if any exist with a matching eoriNumber and channel but no matching mrn" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value
          val otherMovementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori = "eori"
          val arrival = arbitrary[Arrival].sample.value copy(eoriNumber = eori, movementReferenceNumber = otherMovementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, arrival.channel).futureValue

          result mustEqual None
        }
      }

      "must return a None if any exist with a matching mrn and channel but no matching eoriNumber" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori = "eori"
          val otherEori = "otherEori"
          val arrival = arbitrary[Arrival].sample.value copy(eoriNumber = otherEori, movementReferenceNumber = movementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, arrival.channel).futureValue

          result mustEqual None
        }
      }

      "must return a None if any exist with a matching mrn and eori but no matching channel" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori = "eori"
          val arrival = arbitrary[Arrival].sample.value copy(eoriNumber = eori, movementReferenceNumber = movementReferenceNumber, channel = api)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, web).futureValue

          result mustEqual None
        }
      }

      "must return a None when an arrival does not exist" in {
        val app = appBuilder.build()
        running(app) {

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value
          val otherMovementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori = "eori"
          val otherEori = "otherEori"
          val arrival = arbitrary[Arrival].sample.value copy(eoriNumber = otherEori, movementReferenceNumber = otherMovementReferenceNumber, channel = api)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, web).futureValue

          result mustEqual None
        }
      }
    }

    "fetchAllArrivals" - {
      "must return Arrival Movements details that match an eoriNumber and channel type" in {

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = web)

        val app = appBuilder.build()
        running(app) {

          val repository: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val expectedApiFetchAll1 = ResponseArrival(
            arrivalMovement1.arrivalId,
            routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
            routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
            arrivalMovement1.movementReferenceNumber,
            arrivalMovement1.status,
            arrivalMovement1.created,
            arrivalMovement1.lastUpdated
          )

          val expectedApiFetchAll3 = ResponseArrival(
            arrivalMovement3.arrivalId,
            routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
            routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
            arrivalMovement3.movementReferenceNumber,
            arrivalMovement3.status,
            arrivalMovement3.created,
            arrivalMovement3.lastUpdated
          )

          repository.fetchAllArrivals(eoriNumber, api, None).futureValue mustBe Seq(expectedApiFetchAll1)
          repository.fetchAllArrivals(eoriNumber, web, None).futureValue mustBe Seq(expectedApiFetchAll3)
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

          val result = service.fetchAllArrivals(eoriNumber, api, None).futureValue

          result mustBe Seq.empty[Arrival]
        }
      }

      "must filter results by lastUpdated when updatedSince parameter is provided" in {

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = api, lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31))
        val arrivalMovement2 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = api, lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 35, 32))
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = api, lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 21))
        val arrivalMovement4 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = api, lastUpdated = LocalDateTime.of(2021, 4, 30, 10, 15, 16))

        val app = appBuilder.build()
        running(app) {

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val dateTime = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
          val actual = service.fetchAllArrivals(eoriNumber, api, Some(dateTime)).futureValue.toSet
          val expected = Set(arrivalMovement2, arrivalMovement4).map(ResponseArrival.build)

          actual mustEqual expected
        }
      }
    }
  }

  "arrivalsWithoutJsonMessagesSource" - {
    "must return arrivals with any messages that don't have a JSON representation, or whose JSON representation is an empty JSON object" in {
      implicit val actorSystem: ActorSystem = ActorSystem()
      implicit val mat: Materializer = ActorMaterializer()

      val arrival1 = arbitrary[Arrival].map(_.copy(ArrivalId(1))).sample.value
      val arrival2 = arbitrary[Arrival].map(_.copy(ArrivalId(2))).sample.value
      val arrival3 = arbitrary[Arrival].map(_.copy(ArrivalId(3))).sample.value
      val arrival4 = arbitrary[Arrival].map(_.copy(ArrivalId(4))).sample.value

      val messageWithJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj("foo" -> "bar"))
      val messageWithoutJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] - "messageJson"
      val messageWithEmptyJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj())

      val arrivalWithJson = Json.toJson(arrival1).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson))
      val arrivalWithoutJson = Json.toJson(arrival2).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithoutJson))
      val arrivalWithSomeJson = Json.toJson(arrival3).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson, messageWithoutJson))
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
      implicit val mat: Materializer = ActorMaterializer()

      val arrival1 = arbitrary[Arrival].map(_.copy(ArrivalId(1))).sample.value
      val arrival2 = arbitrary[Arrival].map(_.copy(ArrivalId(2))).sample.value
      val arrival3 = arbitrary[Arrival].map(_.copy(ArrivalId(3))).sample.value
      val arrival4 = arbitrary[Arrival].map(_.copy(ArrivalId(4))).sample.value

      val messageWithJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj("foo" -> "bar"))
      val messageWithoutJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] - "messageJson"
      val messageWithEmptyJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj())

      val arrivalWithJson = Json.toJson(arrival1).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson))
      val arrivalWithoutJson = Json.toJson(arrival2).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithoutJson))
      val arrivalWithSomeJson = Json.toJson(arrival3).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson, messageWithoutJson))
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

      val messageWithJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj("foo" -> "bar"))
      val messageWithoutJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] - "messageJson"
      val messageWithEmptyJson = Json.toJson(arbitrary[MovementMessageWithStatus].sample.value).as[JsObject] ++ Json.obj("messageJson" -> Json.obj())

      val arrivalWithJson = Json.toJson(arrival1).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson))
      val arrivalWithoutJson = Json.toJson(arrival2).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithoutJson))
      val arrivalWithSomeJson = Json.toJson(arrival3).as[JsObject] ++ Json.obj("messages" -> Json.arr(messageWithJson, messageWithoutJson))
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
        result.exists(arrival => arrival.arrivalId == arrival1.arrivalId) mustEqual false
        result.exists(arrival => arrival.arrivalId == arrival2.arrivalId) mustEqual true
        result.exists(arrival => arrival.arrivalId == arrival3.arrivalId) mustEqual true
        result.exists(arrival => arrival.arrivalId == arrival4.arrivalId) mustEqual true
      }
    }
  }

  ".resetMessages" - {

    "must replace the messages of an arrival with the newly-supplied ones" in {

      val arrival = arbitrary[Arrival].sample.value
      val message1 = arbitrary[MovementMessageWithStatus].sample.value
      val message2 = arbitrary[MovementMessageWithStatus].sample.value
      val newMessages = NonEmptyList(message1, List(message2))

      val app = appBuilder.build()
      running(app) {
        val repo = app.injector.instanceOf[ArrivalMovementRepository]

        repo.insert(arrival).futureValue

        val resetResult = repo.resetMessages(arrival.arrivalId, newMessages).futureValue
        val updatedArrival = repo.get(arrival.arrivalId).futureValue

        resetResult mustEqual true
        updatedArrival.value mustEqual arrival.copy(messages = newMessages)
      }
    }

    "Must return max 2 arrivals when the API maxRowsReturned = 2" in {
        database.flatMap(_.drop()).futureValue

        val app = new GuiceApplicationBuilder().build()
        val eoriNumber: String = arbitrary[String].sample.value
        val appConfig = app.injector.instanceOf[AppConfig]


        val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)
        val id1 = ArrivalId(1)
        val id2 = ArrivalId(2)
        val id3 = ArrivalId(3)
        val movement1 = arbitrary[Arrival].sample.value.copy(arrivalId = id1, eoriNumber = eoriNumber, channel = api, lastUpdated = lastUpdated.withSecond(10))
        val movement2 = arbitrary[Arrival].sample.value.copy(arrivalId = id2, eoriNumber = eoriNumber, channel = api, lastUpdated = lastUpdated.withSecond(20))
        val movement3 = arbitrary[Arrival].sample.value.copy(arrivalId = id3, eoriNumber = eoriNumber, channel = api, lastUpdated = lastUpdated.withSecond(30))

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]
          repository.insert(movement1).futureValue
          repository.insert(movement2).futureValue
          repository.insert(movement3).futureValue

          val maxRows = appConfig.maxRowsReturned(api)
          maxRows mustBe 2

          val movements = repository.fetchAllArrivals(eoriNumber, api, updatedSince = None).futureValue

          movements.size mustBe maxRows

          val ids = movements.map(m => m.arrivalId.index)

          ids mustBe Seq(movement3.arrivalId.index,movement2.arrivalId.index)

        }
      }

    "Must return max 1 arrivals when the WEB maxRowsReturned = 2" in {
        database.flatMap(_.drop()).futureValue

        val app = new GuiceApplicationBuilder().build()
        val eoriNumber: String = arbitrary[String].sample.value
        val appConfig = app.injector.instanceOf[AppConfig]

        val lastUpdated = LocalDateTime.now(stubClock).withSecond(0).withNano(0)
        val id1 = ArrivalId(11)
        val id2 = ArrivalId(12)
        val id3 = ArrivalId(13)
        val movement1 = arbitrary[Arrival].sample.value.copy(arrivalId = id1, eoriNumber = eoriNumber, channel = web, lastUpdated = lastUpdated.withSecond(1))
        val movement2 = arbitrary[Arrival].sample.value.copy(arrivalId = id2, eoriNumber = eoriNumber, channel = web, lastUpdated = lastUpdated.withSecond(2))
        val movement3 = arbitrary[Arrival].sample.value.copy(arrivalId = id3, eoriNumber = eoriNumber, channel = web, lastUpdated = lastUpdated.withSecond(3))

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[ArrivalMovementRepository]
          repository.insert(movement1).futureValue
          repository.insert(movement2).futureValue
          repository.insert(movement3).futureValue

          val maxRows = appConfig.maxRowsReturned(web)
          maxRows mustBe 100

          val movements = repository.fetchAllArrivals(eoriNumber, web, updatedSince = None).futureValue

          movements.size mustBe 3

          val ids = movements.map( m => m.arrivalId.index)

          ids mustBe Seq(movement3.arrivalId.index, movement2.arrivalId.index, movement1.arrivalId.index)

        }
      }
  }
}