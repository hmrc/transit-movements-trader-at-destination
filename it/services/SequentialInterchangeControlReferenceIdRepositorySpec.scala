/*
 * Copyright 2019 HM Revenue & Customs
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

package it.services

import models.messages.request.InterchangeControlReference
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import reactivemongo.play.json.ImplicitBSONHandlers._
import reactivemongo.play.json.collection.JSONCollection
import repositories.{CollectionNames, SequentialInterchangeControlReferenceIdRepository}
import services.mocks.MockDateTimeService
import services.DateTimeService

import scala.concurrent.ExecutionContext.Implicits.global

class SequentialInterchangeControlReferenceIdRepositorySpec
  extends FreeSpec with MustMatchers with MongoSuite with ScalaFutures with GuiceOneAppPerSuite with IntegrationPatience with MockDateTimeService {

  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .overrides(
      bind[DateTimeService].toInstance(mockTimeService)
    ).build()

  val service: SequentialInterchangeControlReferenceIdRepository = app.injector.instanceOf[SequentialInterchangeControlReferenceIdRepository]

  "SequentialInterchangeControlReferenceIdRepository" - {

    "must generate correct InterchangeControlReference when no record exists within the database" in {

      mockDateFormatted("20190101")

      database.flatMap(_.drop()).futureValue

      val first = service.nextInterchangeControlReferenceId().futureValue

      first mustBe InterchangeControlReference("20190101", 1)

      val second = service.nextInterchangeControlReferenceId().futureValue

      second mustBe InterchangeControlReference("20190101", 2)
    }

    "must generate correct InterchangeControlReference when the collection already has a document in the database" in {

      mockDateFormatted("20190101")

        database.flatMap {
          db =>
            db.drop().flatMap {
              _ =>
                db.collection[JSONCollection](CollectionNames.InterchangeControlReferenceIdsCollection)
                  .insert(ordered = false).one(Json.obj(
                    "_id" -> mockTimeService.dateFormatted,
                    "last-index" -> 1
                  ))
            }
        }.futureValue

        val first = service.nextInterchangeControlReferenceId().futureValue
        val second = service.nextInterchangeControlReferenceId().futureValue

        first.index mustEqual 2
        second.index mustEqual 3
    }

  }

}
