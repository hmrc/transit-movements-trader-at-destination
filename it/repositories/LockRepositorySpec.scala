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

import base.ItSpecBase
import models.ArrivalId
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global

class LockRepositorySpec extends ItSpecBase with FailOnUnindexedQueries with ScalaCheckPropertyChecks {

  "lock" - {
    "must lock an arrivalId when it is not already locked" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        started(app).futureValue

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
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        started(app).futureValue

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
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        started(app).futureValue

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
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[LockRepository]

        repository.unlock(arrivalId).futureValue
      }
    }
  }

  "BSON formatting for ttl index" - {
    "a lock's created field must be a date for the ttl index" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val arrivalId = ArrivalId(1)

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[LockRepository]

        val result = repository.lock(arrivalId).futureValue

        result mustEqual true

        val selector = Json.obj("_id" -> arrivalId, "created" -> Json.obj("$type" -> "date"))

        val lock = database.flatMap(_.collection[JSONCollection](LockRepository.collectionName).find(selector, None).one[JsObject]).futureValue

        lock must be(defined)
      }

    }
  }
}
