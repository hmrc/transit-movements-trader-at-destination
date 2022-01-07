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

import config.AppConfig
import models.LockResult
import models.MongoDateTimeFormats._
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.bson.BSONDocument
import reactivemongo.api.bson.collection.BSONSerializationPack
import reactivemongo.api.commands.LastError
import reactivemongo.api.indexes.Index.Aux
import reactivemongo.api.indexes.IndexType
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection
import utils.IndexUtils

import java.time.Clock
import java.time.LocalDateTime
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class WorkerLockRepository @Inject()(mongo: ReactiveMongoApi, appConfig: AppConfig, clock: Clock)(implicit ec: ExecutionContext) extends Repository {

  private val documentExistsErrorCodeValue = 11000

  private val ttl = appConfig.lockRepositoryTtl

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](WorkerLockRepository.collectionName))

  private val createdIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("created" -> IndexType.Ascending),
    name = Some("created-index"),
    options = BSONDocument("expireAfterSeconds" -> ttl)
  )

  val started: Future[Boolean] =
    collection
      .flatMap {
        _.indexesManager.ensure(createdIndex)
      }
      .map(_ => true)

  def lock(id: String): Future[LockResult] = {

    val lock = Json.obj(
      "_id"     -> id,
      "created" -> LocalDateTime.now(clock)
    )

    collection.flatMap {
      _.insert(ordered = false)
        .one(lock)
        .map(_ => LockResult.LockAcquired)
    } recover {
      case e: LastError if e.code.contains(documentExistsErrorCodeValue) =>
        LockResult.AlreadyLocked
    }
  }

  def unlock(id: String): Future[Boolean] =
    collection.flatMap {
      _.simpleFindAndRemove(
        selector = Json.obj("_id" -> id)
      ).map(_ => true)
    }
}

object WorkerLockRepository {

  val collectionName = "worker-locks"
}
