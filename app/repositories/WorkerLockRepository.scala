/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.Clock
import java.time.LocalDateTime
import config.AppConfig

import javax.inject.Inject
import models.LockResult
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.commands.LastError
import reactivemongo.api.indexes.Index.Aux
import reactivemongo.api.indexes.IndexType
import reactivemongo.bson.BSONDocument
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import models.MongoDateTimeFormats._
import reactivemongo.api.bson.collection.BSONSerializationPack
import utils.IndexUtils

class WorkerLockRepository @Inject()(mongo: ReactiveMongoApi, appConfig: AppConfig, clock: Clock)(implicit ec: ExecutionContext) {

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
      _.findAndRemove(Json.obj("_id" -> id))
        .map(_ => true)
    }
}

object WorkerLockRepository {

  val collectionName = "worker-locks"
}
