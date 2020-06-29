package repositories

import java.time.{LocalDate, LocalDateTime, LocalTime}

import cats.data.NonEmptyList
import generators.ModelGenerators
import models.ArrivalStatus.{ArrivalSubmitted, GoodsReleased, Initialized, UnloadingRemarksSubmitted}
import models.MessageStatus.{SubmissionPending, SubmissionSucceeded}
import models.{Arrival, ArrivalId, ArrivalPutUpdate, ArrivalStatus, MessageId, MessageType, MongoDateTimeFormats, MovementMessageWithStatus, MovementMessageWithoutStatus, MovementReferenceNumber}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.source
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{EitherValues, OptionValues, TryValues}
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.Format

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

class ArrivalMovementRepositorySpec
    extends AnyFreeSpec
    with Matchers
    with FailOnUnindexedQueries
    with IntegrationPatience
    with OptionValues
    with EitherValues
    with TryValues
    with ModelGenerators
    with MongoDateTimeFormats {

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult =>
      throw new TestFailedException((_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
                                    None,
                                    implicitly[source.Position])
  }

  private val eoriNumber: String = arbitrary[String].sample.value

  private def builder = new GuiceApplicationBuilder()

  val arrivalWithOneMessage: Gen[Arrival] = for {
    arrival         <- arbitrary[Arrival]
    movementMessage <- arbitrary[MovementMessageWithStatus]
  } yield arrival.copy(messages = NonEmptyList.one(movementMessage.copy(status = SubmissionPending)))

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

        val arrivalStatus: ArrivalStatus = Initialized
        val arrival                      = arrivalWithOneMessage.sample.value.copy(status = GoodsReleased)

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          repository.updateArrival(ArrivalPutUpdate.selector(arrival.arrivalId), arrivalStatus).futureValue

          val updatedArrival = repository.get(arrival.arrivalId).futureValue.value

          updatedArrival.status mustEqual arrivalStatus

        }
      }

      "must return a Failure if the selector does not match any documents" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrivalStatus: ArrivalStatus = Initialized
        val arrival                      = arrivalWithOneMessage.sample.value copy (arrivalId = ArrivalId(1), status = UnloadingRemarksSubmitted)

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          val result = repository.updateArrival(ArrivalPutUpdate.selector(ArrivalId(2)), arrivalStatus).futureValue

          val updatedArrival = repository.get(arrival.arrivalId).futureValue.value

          result mustBe a[Failure[_]]
          updatedArrival.status must not be (arrivalStatus)
        }
      }
    }

    "setMessageState" - {
      "must update the status of a specific message in an existing arrival" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arrivalWithOneMessage.sample.value

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue

          repository.setMessageState(arrival.arrivalId, 0, SubmissionSucceeded).futureValue

          val updatedArrival = repository.get(arrival.arrivalId).futureValue.value

          typeMatchOnTestValue(updatedArrival.messages.head) {
            result: MovementMessageWithStatus =>
              result.status mustEqual SubmissionSucceeded
          }

        }
      }

      "must fail if the arrival cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arrivalWithOneMessage.sample.value copy (arrivalId = ArrivalId(1))

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.setMessageState(ArrivalId(2), 0, SubmissionSucceeded).futureValue

          result mustBe a[Failure[_]]
        }
      }

      "must fail if the message doesn't exist" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arrivalWithOneMessage.sample.value copy (arrivalId = ArrivalId(1))

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]
          repository.insert(arrival).futureValue

          val result = repository.setMessageState(ArrivalId(1), 5, SubmissionSucceeded).futureValue

          result mustBe a[Failure[_]]
        }
      }

      "must fail if the message does not have a status" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val pregenArrival = arrivalWithOneMessage.sample.value
        val arrival       = pregenArrival.copy(arrivalId = ArrivalId(1), messages = NonEmptyList.one(arbitrary[MovementMessageWithoutStatus].sample.value))

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.setMessageState(ArrivalId(1), 0, SubmissionSucceeded).futureValue

          result mustBe a[Failure[_]]
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

          val updatedArrival = repository.get(arrival.arrivalId).futureValue

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

          val result = repository.get(arrival.arrivalId).futureValue.value
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

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val lastUpdated =  LocalDateTime.now().withSecond(0).withNano(0)
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

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val lastUpdated =  LocalDateTime.now().withSecond(0).withNano(0)
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
          updatedArrival.lastUpdated.withSecond(0).withNano(0)  mustEqual lastUpdated
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

    "get(arrivalId: ArrivalId)" - {
      "must get an arrival when it exists" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val arrival = arbitrary[Arrival].sample.value

          repository.insert(arrival).futureValue
          val result = repository.get(arrival.arrivalId).futureValue

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
          val result = repository.get(ArrivalId(2)).futureValue

          result must not be defined
        }
      }
    }

    "get(eoriNumber: String, mrn: MovementReferenceNumber)" - {
      "must get an arrival if one exists with a matching eoriNumber and mrn" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          val movementReferenceNumber = arbitrary[MovementReferenceNumber].sample.value
          val eori                    = "eori"
          val arrival                 = arbitrary[Arrival].sample.value copy (eoriNumber = eori, movementReferenceNumber = movementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber).futureValue

          result.value mustEqual arrival.copy(lastUpdated = result.value.lastUpdated)
        }
      }

      "must return a None if any exist with a matching eoriNumber but no matching mrn" in {
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

          val result = repository.get(eori, movementReferenceNumber).futureValue

          result mustEqual None
        }
      }

      "must return a None if any exist with a matching mrn but no matching eoriNumber" in {
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

          val result = repository.get(eori, movementReferenceNumber).futureValue

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
          val arrival   = arbitrary[Arrival].sample.value copy (eoriNumber = otherEori, movementReferenceNumber = otherMovementReferenceNumber)

          repository.insert(arrival).futureValue

          val result = repository.get(eori, movementReferenceNumber).futureValue

          result mustEqual None
        }
      }
    }

    "fetchAllArrivals" - {
      "must return Arrival Movements that match an eoriNumber" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrivalMovement1 = arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value

        running(app) {
          started(app).futureValue

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val result: Seq[Arrival] = service.fetchAllArrivals(eoriNumber).futureValue

          result mustBe Seq(arrivalMovement1)
        }
      }

      "must return an empty sequence when there are no movements with the same eori" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()
        val arrivalMovement1 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value

        running(app) {
          started(app).futureValue

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](ArrivalMovementRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val result = service.fetchAllArrivals(eoriNumber).futureValue

          result mustBe Seq.empty[Arrival]
        }
      }

    }
  }

}
