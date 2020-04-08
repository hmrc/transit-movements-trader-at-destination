package services.repositories

import java.time.{LocalDate, LocalDateTime, LocalTime}

import generators.MessageGenerators
import models.MongoDateTimeFormats
import models.Arrival
import models.ArrivalId
import models.MessageType
import models.MovementMessage
import models.MovementReferenceNumber
import models.State
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.EitherValues
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import repositories.ArrivalMovementRepository
import utils.Format

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure
import scala.util.Success

class ArrivalMovementRepositorySpec
    extends FreeSpec
    with MustMatchers
    with MongoSuite
    with FailOnUnindexedQueries
    with ScalaFutures
    with IntegrationPatience
    with OptionValues
    with EitherValues
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with MongoDateTimeFormats {

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

          result.value mustBe arrival
        }
      }
    }

    "setState" - {
      "must update the state of an existing record" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value copy (state = State.PendingSubmission)

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          repository.setState(arrival.arrivalId, State.Submitted).futureValue

          val selector = Json.obj("_id" -> arrival.arrivalId)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](ArrivalMovementRepository.collectionName).find(selector, None).one[Arrival]
          }.futureValue

          result.value.state mustEqual State.Submitted
        }
      }
    }

    "setMessageState" - {
      "must update the state of a specific message in an existing arrival" in {

      }
    }

    "addMessage" - {
      "must add a message and update the state of a document" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value

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

        val goodsReleasedMessage = MovementMessage(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, messageBody)
        val newState             = State.GoodsReleased

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val addMessageResult = repository.addMessage(arrival.arrivalId, goodsReleasedMessage, newState).futureValue

          val selector = Json.obj("_id" -> arrival.arrivalId)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](ArrivalMovementRepository.collectionName).find(selector, None).one[Arrival]
          }.futureValue

          val updatedArrival = result.value

          addMessageResult mustBe a[Success[Unit]]
          updatedArrival.state mustEqual newState
          updatedArrival.messages.size mustEqual arrival.messages.size + 1
          updatedArrival.messages.last mustEqual goodsReleasedMessage
        }
      }

      "must fail if the arrival cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val arrival = arbitrary[Arrival].sample.value copy (state = State.Submitted, arrivalId = ArrivalId(1))

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

        val goodsReleasedMessage = MovementMessage(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, messageBody)
        val newState             = State.GoodsReleased

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[ArrivalMovementRepository]

          repository.insert(arrival).futureValue
          val result = repository.addMessage(ArrivalId(2), goodsReleasedMessage, newState).futureValue

          result mustBe a[Failure[Unit]]
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

          result.value mustEqual arrival
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

          result.value mustEqual arrival
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
