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
import models.LockResult
import models.LockResult.LockAcquired
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.test.Helpers._
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global

class WorkerLockRepositorySpec extends ItSpecBase with FailOnUnindexedQueries with ScalaCheckPropertyChecks {

  "lock" - {
    "must lock an id when it is not already locked" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val id = "foo"

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[WorkerLockRepository]

        val result = repository.lock(id).futureValue

        result mustEqual LockResult.LockAcquired

        val selector = Json.obj("_id" -> id)
        val lock = database.flatMap {
          db =>
            db.collection[JSONCollection](WorkerLockRepository.collectionName).find(selector, None).one[JsObject]
        }.futureValue

        lock.value("_id").validate[String] mustEqual JsSuccess(id)
      }
    }

    "must not lock an id that is already locked" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val id = "foo"

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[WorkerLockRepository]

        val result1 = repository.lock(id).futureValue
        val result2 = repository.lock(id).futureValue

        result1 mustEqual LockResult.LockAcquired
        result2 mustEqual LockResult.AlreadyLocked
      }
    }
  }

  "unlock" - {
    "must remove an existing lock" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val id = "foo"

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[WorkerLockRepository]

        repository.lock(id).futureValue
        repository.unlock(id).futureValue

        val selector = Json.obj("_id" -> id)
        val remainingLock = database.flatMap {
          db =>
            db.collection[JSONCollection](WorkerLockRepository.collectionName).find(selector, None).one[JsObject]
        }.futureValue

        remainingLock must not be defined
      }
    }

    "must not fail when asked to remove a lock that doesn't exist" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val id = "foo"

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[WorkerLockRepository]

        repository.unlock(id).futureValue
      }
    }
  }

  "BSON formatting for ttl index" - {
    "a lock's created field must be a date for the ttl index" in {
      database.flatMap(_.drop()).futureValue

      val app = new GuiceApplicationBuilder().build()

      val id = "foo"

      running(app) {
        started(app).futureValue

        val repository = app.injector.instanceOf[WorkerLockRepository]

        val result = repository.lock(id).futureValue

        result mustEqual LockAcquired

        val selector = Json.obj("_id" -> id, "created" -> Json.obj("$type" -> "date"))

        val lock = database.flatMap(_.collection[JSONCollection](WorkerLockRepository.collectionName).find(selector, None).one[JsObject]).futureValue

        lock must be(defined)
      }

    }
  }
}
