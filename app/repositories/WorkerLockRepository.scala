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
import config.AppConfig
import models.ArrivalId
import models.LockResult
import models.MongoDateTimeFormats._
import org.mongodb.scala.model.IndexModel
import org.mongodb.scala.model.Indexes
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import utils.IndexUtils

import java.time.Clock
import java.time.LocalDateTime
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[WorkerLockRepositoryImpl])
trait WorkerLockRepository {
  def lock(id: String): Future[LockResult]
  def unlock(id: String): Future[Boolean]
}

//@Singleton
class WorkerLockRepositoryImpl @Inject()(mongo: MongoComponent, appConfig: AppConfig, clock: Clock)(implicit ec: ExecutionContext)
    extends PlayMongoRepository(
      mongoComponent = mongo,
      collectionName = "worker-locks",
      domainFormat = ArrivalId.formatsArrivalId,
      indexes = Seq(IndexModel(Indexes.ascending("created")))
    )
    with WorkerLockRepository {

//  private val documentExistsErrorCodeValue = 11000
//
//  private val ttl = appConfig.lockRepositoryTtl
//
//  private def collection: Future[JSONCollection] =
//    mongo.database.map(_.collection[JSONCollection](WorkerLockRepository.collectionName))
//
//  private val createdIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
//    key = Seq("created" -> IndexType.Ascending),
//    name = Some("created-index"),
//    options = BSONDocument("expireAfterSeconds" -> ttl)
//  )

  val started: Future[Boolean] = Future.successful(true)
//    collection
//      .flatMap {
//        _.indexesManager.ensure(createdIndex)
//      }
//      .map(
//        _ => true
//      )

  def lock(id: String): Future[LockResult] = Future.successful(LockResult.LockAcquired)
//  {
//
//    val lock = Json.obj(
//      "_id"     -> id,
//      "created" -> LocalDateTime.now(clock)
//    )
//
//    collection.flatMap {
//      _.insert(ordered = false)
//        .one(lock)
//        .map(
//          _ => LockResult.LockAcquired
//        )
//    } recover {
//      case e: LastError if e.code.contains(documentExistsErrorCodeValue) =>
//        LockResult.AlreadyLocked
//    }
//  }

  def unlock(id: String): Future[Boolean] = Future.successful(true)
//    collection.flatMap {
//      _.simpleFindAndRemove(
//        selector = Json.obj("_id" -> id)
//      ).map(
//        _ => true
//      )
//    }
}
