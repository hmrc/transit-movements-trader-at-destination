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

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import base._
import models.ArrivalStatus.ArrivalSubmitted
import models.ArrivalStatus.GoodsReleased
import models.ArrivalStatus.Initialized
import models.ArrivalStatus.UnloadingRemarksSubmitted
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.Arrival
import models.ArrivalId
import models.ArrivalIdSelector
import models.ArrivalStatus
import models.ArrivalStatusUpdate
import models.ChannelType.{api, web}
import models.MessageId
import models.MessageType
import models.MongoDateTimeFormats
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.source
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.Format

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success

class ArrivalMovementRepositorySpec extends ItSpecBase with FailOnUnindexedQueries with MongoDateTimeFormats {

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult =>
      throw new TestFailedException((_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
                                    None,
                                    implicitly[source.Position])
  }

  private val eoriNumber: String = arbitrary[String].sample.value

  private def builder = new GuiceApplicationBuilder()

  "ArrivalMovementRepository" - {
    "insert" - {
      "must persist ArrivalMovement within mongoDB" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value

        running(app) {
          started(app).futureValue

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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrivalStatus = ArrivalStatusUpdate(Initialized)
        val arrival       = arrivalWithOneMessage.sample.value.copy(status = GoodsReleased)
        val latsUpdated   = LocalDateTime.now.withSecond(0).withNano(0)
        val selector      = ArrivalIdSelector(arrival.arrivalId)

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          repository.updateArrival(selector, arrivalStatus).futureValue

          val updatedArrival = repository.get(arrival.arrivalId, arrival.channel).futureValue.value

          updatedArrival.status mustEqual arrivalStatus.arrivalStatus
          updatedArrival.lastUpdated.withSecond(0).withNano(0) mustEqual latsUpdated

        }
      }

      "must return a Failure if the selector does not match any documents" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrivalStatus = ArrivalStatusUpdate(Initialized)
        val arrival       = arrivalWithOneMessage.sample.value copy (arrivalId = ArrivalId(1), status = UnloadingRemarksSubmitted)
        val selector      = ArrivalIdSelector(ArrivalId(2))

        running(app) {
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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival   = arrivalWithOneMessage.sample.value.copy(status = ArrivalStatus.Initialized)
        val messageId = MessageId.fromIndex(0)

        running(app) {
          started(app).futureValue

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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival   = arrivalWithOneMessage.sample.value.copy(arrivalId = ArrivalId(1), status = Initialized)
        val messageId = MessageId.fromIndex(0)

        running(app) {
          started(app).futureValue

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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value

        val dateOfPrep  = LocalDate.now()
        val timeOfPrep  = LocalTime.of(1, 1)
        val lastUpdated = LocalDateTime.now().withSecond(0).withNano(0)
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

        running(app) {
          started(app).futureValue

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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value copy (status = ArrivalStatus.ArrivalSubmitted, arrivalId = ArrivalId(1))

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
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

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addResponseMessage(ArrivalId(2), goodsReleasedMessage, newState).futureValue

          result mustBe a[Failure[_]]
        }
      }
    }

    "addNewMessage" - {
      "must add a message, update the timestamp and increment nextCorrelationId" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value.copy(status = ArrivalStatus.ArrivalSubmitted)

        val dateOfPrep  = LocalDate.now()
        val timeOfPrep  = LocalTime.of(1, 1)
        val lastUpdated = LocalDateTime.now().withSecond(0).withNano(0)
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

        running(app) {
          started(app).futureValue

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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value copy (status = ArrivalStatus.ArrivalSubmitted, arrivalId = ArrivalId(1))

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
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

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addNewMessage(ArrivalId(2), goodsReleasedMessage).futureValue

          result mustBe a[Failure[_]]
        }
      }

    }

    "get(arrivalId: ArrivalId, channelFilter: ChannelType)" - {
      "must get an arrival when it exists and has the right channel type" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value

          repository.insert(arrival).futureValue
          val result = repository.get(arrival.arrivalId, arrival.channel).futureValue

          result.value mustEqual arrival.copy(lastUpdated = result.value.lastUpdated)
        }
      }

      "must return None when an arrival does not exist" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

          repository.insert(arrival).futureValue
          val result = repository.get(ArrivalId(2), arrival.channel).futureValue

          result must not be defined
        }
      }

      "must return None when an arrival does exist but with the wrong channel type" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

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
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value
          val eori                    = "eori"
          val arrival                 = arbitrary[Arrival].sample.value copy (eoriNumber = eori, movementReferenceNumber = movementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, arrival.channel).futureValue

          result.value mustEqual arrival.copy(lastUpdated = result.value.lastUpdated)
        }
      }

      "must return a None if any exist with a matching eoriNumber and channel but no matching mrn" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber      = arbitrary[MovementReferenceNumber].sample.value
          val otherMovementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori    = "eori"
          val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = eori, movementReferenceNumber = otherMovementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, arrival.channel).futureValue

          result mustEqual None
        }
      }

      "must return a None if any exist with a matching mrn and channel but no matching eoriNumber" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori      = "eori"
          val otherEori = "otherEori"
          val arrival   = arbitrary[Arrival].sample.value copy (eoriNumber = otherEori, movementReferenceNumber = movementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, arrival.channel).futureValue

          result mustEqual None
        }
      }

      "must return a None if any exist with a matching mrn and eori but no matching channel" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori      = "eori"
          val arrival   = arbitrary[Arrival].sample.value copy (eoriNumber = eori, movementReferenceNumber = movementReferenceNumber, channel = api)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, web).futureValue

          result mustEqual None
        }
      }

      "must return a None when an arrival does not exist" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber      = arbitrary[MovementReferenceNumber].sample.value
          val otherMovementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value

          val eori      = "eori"
          val otherEori = "otherEori"
          val arrival   = arbitrary[Arrival].sample.value copy (eoriNumber = otherEori, movementReferenceNumber = otherMovementReferenceNumber, channel = api)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber, web).futureValue

          result mustEqual None
        }
      }
    }

    "fetchAllArrivals" - {
      "must return Arrival Movements that match an eoriNumber and channel type" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, channel = web)

        running(app) {
          started(app).futureValue

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          service.fetchAllArrivals(eoriNumber, api).futureValue mustBe Seq(arrivalMovement1)
          service.fetchAllArrivals(eoriNumber, web).futureValue mustBe Seq(arrivalMovement3)
        }
      }

      "must return an empty sequence when there are no movements with the same eori" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()
        val arrivalMovement1 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)

        running(app) {
          started(app).futureValue

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val result = service.fetchAllArrivals(eoriNumber, api).futureValue

          result mustBe Seq.empty[Arrival]
        }
      }

    }
  }

}
