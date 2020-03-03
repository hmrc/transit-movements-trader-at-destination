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
import com.google.inject.Inject
import models.request.InternalReferenceId
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection.JSONCollection
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

class InternalReferenceIdRepository @Inject()(mongo: ReactiveMongoApi) extends InternalReferenceIdRepositoryInterface {

  private val lastIndexKey = "last-index"
  private val primaryValue = "record_id"

  private val collectionName: String = CollectionNames.MovementReferenceIdsCollection

  private val indexKeyReads: Reads[Int] = {
    import play.api.libs.json._
    (__ \ lastIndexKey).read[Int]
  }

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def nextId(): Future[InternalReferenceId] = {

    val update = Json.obj(
      "$inc" -> Json.obj(lastIndexKey -> 1)
    )

    val selector = Json.obj("_id" -> primaryValue)

    collection.flatMap(
      _.findAndUpdate(selector, update, upsert = true, fetchNewObject = true)
        .map(
          x =>
            x.result(indexKeyReads)
              .map(increment => InternalReferenceId(increment))
              .getOrElse(throw new Exception(s"Unable to generate MovementReferenceId")))
    )

  }

}

trait InternalReferenceIdRepositoryInterface {
  def nextId(): Future[InternalReferenceId]
}

sealed trait FailedCreatingNextInternalReferenceId

object FailedCreatingNextInternalReferenceId extends FailedCreatingNextInternalReferenceId
