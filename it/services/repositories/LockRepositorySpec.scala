package services.repositories

import generators.MessageGenerators
import models.request.ArrivalId
import org.scalatest.{EitherValues, FreeSpec, MustMatchers, OptionValues}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsObject, JsSuccess, Json}
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import repositories.LockRepository
import services.mocks.MockDateTimeService

import scala.concurrent.ExecutionContext.Implicits.global

class LockRepositorySpec
  extends FreeSpec
    with MustMatchers
    with MongoSuite
    with FailOnUnindexedQueries
    with ScalaFutures
    with IntegrationPatience
    with MockDateTimeService
    with OptionValues
    with EitherValues
    with ScalaCheckPropertyChecks
    with MessageGenerators {

  override def beforeEach(): Unit = {
    super.beforeEach()
    database.flatMap(_.drop()).futureValue
  }

  "lock" - {
    "must lock an arrivalId when it is not already locked" in {

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        val repository = app.injector.instanceOf[LockRepository]

        val result = repository.lock(arrivalId).futureValue

        result mustEqual true

        val selector = Json.obj("_id" -> arrivalId)
        val lock = database.flatMap {
          db =>
            db.collection[JSONCollection](LockRepository.collectionName).find(selector, None).one[JsObject]
        }.futureValue

        lock.value("_id").validate[ArrivalId] mustEqual JsSuccess(arrivalId)
      }
    }

    "must not lock an arrivalId that is already locked" in {

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        val repository = app.injector.instanceOf[LockRepository]

        val result1 = repository.lock(arrivalId).futureValue
        val result2 = repository.lock(arrivalId).futureValue

        result1 mustEqual true
        result2 mustEqual false
      }
    }
  }

  "unlock" - {
    "must remove an existing lock" in {

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        val repository = app.injector.instanceOf[LockRepository]

        repository.lock(arrivalId).futureValue
        repository.unlock(arrivalId).futureValue

        val selector = Json.obj("_id" -> arrivalId)
        val remainingLock = database.flatMap {
          db =>
            db.collection[JSONCollection](LockRepository.collectionName).find(selector, None).one[JsObject]
        }.futureValue

        remainingLock must not be defined
      }
    }

    "must not fail when asked to remove a lock that doesn't exist" in {

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        val repository = app.injector.instanceOf[LockRepository]

        repository.unlock(arrivalId).futureValue
      }
    }
  }
}
