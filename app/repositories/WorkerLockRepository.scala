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
import com.mongodb.MongoWriteException
import config.AppConfig
import models.ArrivalWorkerLock
import models.LockResult
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.IndexModel
import org.mongodb.scala.model.IndexOptions
import org.mongodb.scala.model.Indexes
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.time.Clock
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[WorkerLockRepositoryImpl])
trait WorkerLockRepository {
  def lock(id: String): Future[LockResult]
  def unlock(id: String): Future[Boolean]
  val started: Future[Boolean]
}

//@Singleton
class WorkerLockRepositoryImpl @Inject() (mongo: MongoComponent, appConfig: AppConfig, clock: Clock)(implicit ec: ExecutionContext)
    extends PlayMongoRepository(
      mongoComponent = mongo,
      collectionName = "worker-locks-hmrc-mongo",
      domainFormat = ArrivalWorkerLock.format,
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
      )
    )
    with WorkerLockRepository {

  private val documentExistsErrorCodeValue = 11000

  override val started: Future[Boolean] = ensureIndexes.map(
    _ => true
  )

  def lock(id: String): Future[LockResult] =
    collection
      .insertOne(ArrivalWorkerLock(id, LocalDateTime.now()))
      .head()
      .map(
        _ => LockResult.LockAcquired
      ) recover {
      case e: MongoWriteException if e.getError.getCode == documentExistsErrorCodeValue =>
        LockResult.AlreadyLocked
    }

  def unlock(id: String): Future[Boolean] =
    collection
      .findOneAndDelete(Filters.eq("_id", id))
      .toFuture()
      .map(
        _ => true
      )

}
