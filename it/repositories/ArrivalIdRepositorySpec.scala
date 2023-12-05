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

//import config.AppConfig
import models.ArrivalId
import models.ArrivalIdWrapper
//import org.mockito.Mockito
//import org.scalatest.concurrent.IntegrationPatience
//import org.scalatest.concurrent.ScalaFutures
//import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
//import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
//import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.Configuration
import play.api.Logging
import play.api.inject.guice.GuiceApplicationBuilder
//import play.api.libs.json.Json
import play.api.test.DefaultAwaitTimeout
import play.api.test.FutureAwaits
import uk.gov.hmrc.mongo.MongoComponent
//import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalIdRepositorySpec
    extends AnyFreeSpec
    with Matchers
    with FutureAwaits
    with DefaultAwaitTimeout
    with Logging
    with DefaultPlayMongoRepositorySupport[ArrivalIdWrapper] {

  implicit lazy val app: Application = GuiceApplicationBuilder()
    .configure(
      "play.http.router"               -> "testOnlyDoNotUseInAppConf.Routes",
      "feature-flags.testOnly.enabled" -> false
    )
    .build()

  private val config = app.injector.instanceOf[Configuration]

  override protected def repository = new ArrivalIdRepositoryImpl(mongoComponent, config)

  "ArrivalIdRepository" - {

    "must generate sequential ArrivalIds starting at 1 when no record exists within the database" in {
      val first  = repository.nextId().futureValue
      val second = repository.nextId().futureValue

      first mustBe ArrivalId(1)
      second mustBe ArrivalId(2)
    }

    "must generate sequential ArrivalIds when a record exists within the database" in {

      repository.collection.insertOne(ArrivalIdWrapper(1)).toFuture()
      val first  = repository.nextId().futureValue
      val second = repository.nextId().futureValue

      first mustBe ArrivalId(2)
      second mustBe ArrivalId(3)

    }
  }
}
