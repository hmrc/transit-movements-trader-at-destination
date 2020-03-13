package services.repositories

import generators.MessageGenerators
import models.ArrivalMovement
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import repositories.{ArrivalMovementRepository, CollectionNames}
import services.mocks.MockDateTimeService

import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalMovementRepositorySpec extends FreeSpec with MustMatchers with MongoSuite with FailOnUnindexedQueries with ScalaFutures with GuiceOneAppPerSuite with IntegrationPatience with MockDateTimeService with OptionValues with ScalaCheckPropertyChecks with MessageGenerators {


  private lazy val builder = new GuiceApplicationBuilder()

  "ArrivalMovementRepository" - {
    "must persist ArrivalMovement within mongoDB" in {
      val arrivalMovement = arbitrary[ArrivalMovement].sample.value
      val app: Application = builder.build()

      running(app) {
        started(app).futureValue

        val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

        service.persistToMongo(arrivalMovement).futureValue

        val selector = Json.obj("eoriNumber" -> arrivalMovement.eoriNumber)

        val getValue: Option[ArrivalMovement] = database.flatMap { result =>
          result.collection[JSONCollection](CollectionNames.ArrivalMovementCollection).find(selector, None).one[ArrivalMovement]
        }.futureValue

        getValue.value mustBe arrivalMovement
      }
    }

    "fetchAllMovements" - {
      "must fetch all the ArrivalMovements from mongoDB and filter non-matching eori's" in {
        val app: Application = builder.build()

        val arrivalMovement = arbitrary[ArrivalMovement].sample.value
        val arrivalMovementWithDifferentEori = arbitrary[ArrivalMovement].sample.value

        running(app) {
          started(app).futureValue

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement, arrivalMovementWithDifferentEori)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap { db =>
            db.collection[JSONCollection](CollectionNames.ArrivalMovementCollection).insert(false).many(jsonArr)
          }.futureValue

          val result: Seq[ArrivalMovement] = service.fetchAllMovements(arrivalMovement.eoriNumber).futureValue

          result mustBe Seq(arrivalMovement)
        }
      }

      "must return an empty sequence when there are no movements with the same eori" in {
        val app: Application = builder.build()

        val arrivalMovement1 = arbitrary[ArrivalMovement].sample.value
        val arrivalMovement2 = arbitrary[ArrivalMovement].sample.value

        val eoriNumber = arbitrary[String].sample.value

        running(app) {
          started(app).futureValue

          val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

          val allMovements = Seq(arrivalMovement1, arrivalMovement2)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap { db =>
            db.collection[JSONCollection](CollectionNames.ArrivalMovementCollection).insert(false).many(jsonArr)
          }.futureValue

          val result: Seq[ArrivalMovement] = service.fetchAllMovements(eoriNumber).futureValue

          result mustBe Seq.empty[ArrivalMovement]
        }
      }
    }
  }

}

