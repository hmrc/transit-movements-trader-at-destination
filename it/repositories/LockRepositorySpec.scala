package repositories

import generators.ModelGenerators
import models.ArrivalId
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.EitherValues
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global

class LockRepositorySpec
    extends FreeSpec
    with MustMatchers
    with MongoSuite
    with FailOnUnindexedQueries
    with ScalaFutures
    with IntegrationPatience
    with OptionValues
    with EitherValues
    with ScalaCheckPropertyChecks
    with ModelGenerators {

  "lock" - {
    "must lock an arrivalId when it is not already locked" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        val repository = app.injector.instanceOf[LockRepository]

        repository.started.futureValue

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
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        val repository = app.injector.instanceOf[LockRepository]

        repository.started.futureValue

        val result1 = repository.lock(arrivalId).futureValue
        val result2 = repository.lock(arrivalId).futureValue

        result1 mustEqual true
        result2 mustEqual false
      }
    }
  }

  "unlock" - {
    "must remove an existing lock" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {

        val repository = app.injector.instanceOf[LockRepository]

        repository.started.futureValue

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
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {

        val repository = app.injector.instanceOf[LockRepository]

        repository.started.futureValue

        repository.unlock(arrivalId).futureValue
      }
    }
  }
}
