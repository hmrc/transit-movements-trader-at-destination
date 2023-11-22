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

import com.google.inject._
import com.mongodb.client.model.Filters
import com.mongodb.client.model.Updates
import models.ArrivalId
import org.mongodb.scala.model.FindOneAndUpdateOptions
import org.mongodb.scala.model._
import play.api.Configuration
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json._

import scala.concurrent._

@ImplementedBy(classOf[ArrivalIdRepositoryImpl])
trait ArrivalIdRepository {
  def setNextId(nextId: Int): Future[Unit]
  def nextId(): Future[ArrivalId]
}

@Singleton
class ArrivalIdRepositoryImpl @Inject()(mongo: MongoComponent, config: Configuration)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[ArrivalId](
      mongoComponent = mongo,
      collectionName = "arrival-ids-hmrc-mongo",
      domainFormat = ArrivalId.formatsArrivalId,
      indexes = Seq(IndexModel(Indexes.ascending("last-index"))),
      extraCodecs = Seq(
        Codecs.playFormatCodec(ArrivalId.formatsArrivalId) //TODO: needed?
      )
    )
    with ArrivalIdRepository {

  val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  private val lastIndexKey = "last-index"
  private val primaryValue = "record_id"

  def setNextId(nextId: Int): Future[Unit] =
    if (featureFlag) {
      val update   = Updates.set(lastIndexKey, nextId)
      val selector = Filters.eq("_id", primaryValue)
      collection
        .updateOne(selector, update)
        .toFuture
        .map {
          result =>
            if (result.wasAcknowledged()) {
              if (result.getModifiedCount == 0)
                Future.failed(new Exception("No document modified: count is zero"))
              else
                Future.successful(())
            } else
              Future.failed(new Exception("Unable to set next ArrivalId"))
        }
    } else
      Future.failed(new Exception("Feature disabled, cannot set next ArrivalId"))

  def nextId(): Future[ArrivalId] = {
    val update   = Updates.inc(lastIndexKey, 1)
    val selector = Filters.eq("_id", primaryValue)
    collection
      .findOneAndUpdate(selector, update, FindOneAndUpdateOptions().upsert(true))
      .toFuture()
  }

}
