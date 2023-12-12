/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import models.ArrivalWorkerLock
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.time.ZoneId
import java.time.Clock
import config.AppConfig
import com.mongodb.client.model.Filters

class WorkerLockRepositorySpec
    extends ItSpecBase
    with GuiceOneAppPerSuite
    with ScalaCheckPropertyChecks
    with DefaultPlayMongoRepositorySupport[ArrivalWorkerLock] {

  private val appConfig                 = app.injector.instanceOf[AppConfig]
  private val instant                   = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  implicit private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  override lazy val repository = new WorkerLockRepositoryImpl(mongoComponent, appConfig, stubClock)

  "lock" - {
    "must lock an id when it is not already locked" in {
      val id     = "foo"
      val result = repository.lock(id).futureValue

      result mustEqual LockResult.LockAcquired

      val selector = Filters.eq("_id", id)
      val lock     = repository.collection.find(selector).head().futureValue

      lock.id mustEqual id
    }

    "must not lock an id that is already locked" in {
      val id = "foo"

      val result1 = repository.lock(id).futureValue
      val result2 = repository.lock(id).futureValue

      result1 mustEqual LockResult.LockAcquired
      result2 mustEqual LockResult.AlreadyLocked
    }
  }

  "unlock" - {
    "must remove an existing lock" in {
      val id = "foo"

      repository.lock(id).futureValue
      repository.unlock(id).futureValue

      val selector      = Filters.eq("_id", id)
      val remainingLock = repository.collection.find(selector).headOption().futureValue

      remainingLock mustEqual None
    }

    "must not fail when asked to remove a lock that doesn't exist" in {
      val id = "foo"
      repository.unlock(id).futureValue
    }
  }

  "BSON formatting for ttl index" - {
    "a lock's created field must be a date for the ttl index" in {
      val id = "foo"

      val result = repository.lock(id).futureValue

      result mustEqual LockAcquired

      val selector = Filters.and(Filters.eq("_id", id), Filters.`type`("created", "date"))

      val lock = repository.collection.find(selector).head().futureValue

      lock.id mustEqual id
    }

  }
}
