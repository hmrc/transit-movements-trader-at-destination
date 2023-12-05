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
import com.mongodb.client.model.Filters
import config.AppConfig
import models.ArrivalId
import models.ArrivalLock
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import java.time._
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext.Implicits.global

class LockRepositorySpec extends ItSpecBase with GuiceOneAppPerSuite with ScalaCheckPropertyChecks with DefaultPlayMongoRepositorySupport[ArrivalLock] {

  private val appConfig                 = app.injector.instanceOf[AppConfig]
  private val instant                   = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  implicit private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  override lazy val repository = new LockRepositoryImpl(appConfig, mongoComponent, stubClock)

  "lock" - {
    "must lock an arrivalId when it is not already locked" in {
      val arrivalId = ArrivalId(1)
      val result    = repository.lock(arrivalId).futureValue

      result mustEqual true

      val selector = Filters.eq("_id", arrivalId.index.toString)

      val lock = repository.collection.find(selector).head().futureValue

      lock.id mustEqual arrivalId.index.toString
    }
  }

  "must not lock an arrivalId that is already locked" in {

    val arrivalId = ArrivalId(1)

    val result1 = repository.lock(arrivalId).futureValue
    val result2 = repository.lock(arrivalId).futureValue

    result1 mustEqual true
    result2 mustEqual false
  }

  "unlock" - {
    "must remove an existing lock" in {

      val arrivalId = ArrivalId(1)

      repository.lock(arrivalId).futureValue
      repository.unlock(arrivalId).futureValue

      val selector      = Filters.eq("_id", arrivalId.index.toString)
      val remainingLock = repository.collection.find(selector).headOption().futureValue
      remainingLock mustEqual None
    }

    "must not fail when asked to remove a lock that doesn't exist" in {
      val arrivalId = ArrivalId(1)
      val result    = repository.unlock(arrivalId)

      whenReady(result) {
        _ mustBe true
      }
    }
  }

  "BSON formatting for ttl index" - {
    "a lock's created field must be a date for the ttl index" in {

      val arrivalId = ArrivalId(1)
      val result    = repository.lock(arrivalId).futureValue

      result mustEqual true

      val selector = Filters.and(Filters.eq("_id", arrivalId.index.toString), Filters.`type`("created", "date"))

      val lock = repository.collection.find(selector).head().futureValue

      lock.id mustEqual arrivalId.index.toString
    }
  }
}
