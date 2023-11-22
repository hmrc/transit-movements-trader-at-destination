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

import config.AppConfig
import models.ArrivalId
import org.mockito.Mockito
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.Configuration
import play.api.Logging
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.DefaultAwaitTimeout
import play.api.test.FutureAwaits
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalIdRepositorySpec
    extends AnyFreeSpec
    with Matchers
    with FutureAwaits
    with DefaultAwaitTimeout
    with Logging
    with DefaultPlayMongoRepositorySupport[ArrivalId] {

  override lazy val mongoComponent: MongoComponent = {
    val databaseName: String = "arrival-ids-hmrc-mongo-test"
    val mongoUri: String     = s"mongodb://localhost:27017/$databaseName?retryWrites=false"
    MongoComponent(mongoUri)
  }

  implicit lazy val app: Application = GuiceApplicationBuilder()
    .configure("feature-flags.testOnly.enabled" -> true)
    .build()

  private val config = app.injector.instanceOf[Configuration]

  override protected def repository = new ArrivalIdRepositoryImpl(mongoComponent, config)

  override def beforeEach(): Unit = repository.collection.drop()

  "ArrivalIdRepository" - {

    "should have the correct name" in {
      repository.collectionName shouldBe "arrival-ids"
    }

    "must generate sequential ArrivalIds starting at 1 when no record exists within the database" in {
      val first  = repository.nextId().futureValue
      val second = repository.nextId().futureValue

      first mustBe ArrivalId(1)
      second mustBe ArrivalId(2)
    }

    "must generate sequential ArrivalIds when a record exists within the database" in {

      repository.collection.drop().flatMap {
        _ => repository.collection.insertOne(ArrivalId(12))
      }

      val first  = repository.nextId().futureValue
      val second = repository.nextId().futureValue

      first mustBe ArrivalId(13)
      second mustBe ArrivalId(14)
    }
  }
}
