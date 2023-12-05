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

import com.google.inject.ImplementedBy
import com.google.inject.Singleton
import com.mongodb.MongoWriteException
import config.AppConfig
import models.ArrivalId
import models.ArrivalLock
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.IndexModel
import org.mongodb.scala.model.IndexOptions
import org.mongodb.scala.model.Indexes
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.time.Clock
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[LockRepositoryImpl])
trait LockRepository {
  def lock(arrivalId: ArrivalId): Future[Boolean]
  def unlock(arrivalId: ArrivalId): Future[Boolean]
  val started: Future[Unit]
}

@Singleton
class LockRepositoryImpl @Inject()(appConfig: AppConfig, mongo: MongoComponent, clock: Clock)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[ArrivalLock](
      mongoComponent = mongo,
      collectionName = "locks-hmrc-mongo",
      domainFormat = ArrivalLock.format,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("created"),
          IndexOptions()
            .name("created-index")
            .expireAfter(appConfig.lockRepositoryTtl, TimeUnit.SECONDS)
            .unique(false)
            .sparse(false)
            .background(false)
        )
      ),
      extraCodecs = Seq(Codecs.playFormatCodec(ArrivalId.formatsArrivalId))
    )
    with LockRepository {

  private val documentExistsErrorCodeValue = 11000

  override val started: Future[Unit] = ensureIndexes.map(
    _ => ()
  )

  def lock(arrivalId: ArrivalId): Future[Boolean] =
    collection
      .insertOne(ArrivalLock(arrivalId.index.toString, LocalDateTime.now(clock)))
      .head()
      .map(
        result => result.wasAcknowledged()
      ) recover {
      case e: MongoWriteException if e.getError.getCode == documentExistsErrorCodeValue =>
        false
    }

  def unlock(arrivalId: ArrivalId): Future[Boolean] =
    collection
      .findOneAndDelete(Filters.eq("_id", arrivalId.index.toString))
      .toFuture()
      .map(
        _ => true
      )

}
