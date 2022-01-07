/*
 * Copyright 2022 HM Revenue & Customs
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

import models.ArrivalId
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalIdRepositorySpec extends AnyFreeSpec with Matchers with ScalaFutures with MongoSuite with GuiceOneAppPerSuite with IntegrationPatience {

  private val service = app.injector.instanceOf[ArrivalIdRepository]

  "ArrivalIdRepository" - {

    "must generate sequential ArrivalIds starting at 1 when no record exists within the database" in {

      database.flatMap(_.drop()).futureValue

      val first  = service.nextId().futureValue
      val second = service.nextId().futureValue

      first mustBe ArrivalId(1)
      second mustBe ArrivalId(2)
    }

    "must generate sequential ArrivalIds when a record exists within the database" in {

      database.flatMap {
        db =>
          db.drop().flatMap {
            _ =>
              db.collection[JSONCollection](ArrivalIdRepository.collectionName)
                .insert(ordered = false)
                .one(
                  Json.obj(
                    "_id"        -> "record_id",
                    "last-index" -> 1
                  ))
          }
      }.futureValue

      val first  = service.nextId().futureValue
      val second = service.nextId().futureValue

      first mustBe ArrivalId(2)
      second mustBe ArrivalId(3)
    }
  }
}
