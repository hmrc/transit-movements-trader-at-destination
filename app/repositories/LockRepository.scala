/*
 * Copyright 2020 HM Revenue & Customs
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

import javax.inject.Inject
import models.request.ArrivalId
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.commands.LastError
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class LockRepository @Inject()(mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) {

  private val documentExistsErrorCodeValue = 11000

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](LockRepository.collectionName))

  def lock(arrivalId: ArrivalId): Future[Boolean] = {

    val lock = Json.obj(
      "_id" -> arrivalId
    )

    collection.flatMap {
      _.insert(ordered = false)
        .one(lock)
        .map(_ => true)
    } recover {
      case e: LastError if e.code.contains(documentExistsErrorCodeValue) =>
        false
    }
  }

  def unlock(arrivalId: ArrivalId): Future[Unit] =
    collection.flatMap {
      _.findAndRemove(Json.obj("_id" -> arrivalId))
        .map(_ => ())
    }
}

object LockRepository {

  val collectionName = "locks"
}
