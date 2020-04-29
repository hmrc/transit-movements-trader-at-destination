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
import models.ArrivalId
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection.JSONCollection
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

class ArrivalIdRepository @Inject()(mongo: ReactiveMongoApi) {

  private val lastIndexKey = "last-index"
  private val primaryValue = "record_id"

  private val collectionName: String = ArrivalIdRepository.collectionName

  private val indexKeyReads: Reads[ArrivalId] = {
    import play.api.libs.json._
    (__ \ lastIndexKey).read[ArrivalId]
  }

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def nextId(): Future[ArrivalId] = {

    import reactivemongo.api.WriteConcern

    val update = Json.obj(
      "$inc" -> Json.obj(lastIndexKey -> 1)
    )

    val selector = Json.obj("_id" -> primaryValue)

    collection.flatMap(
      _.findAndUpdate(
        selector = selector,
        update = update,
        fetchNewObject = true,
        upsert = true,
        sort = Option.empty,
        fields = Option.empty,
        bypassDocumentValidation = false,
        writeConcern = WriteConcern.Default,
        maxTime = Option.empty,
        collation = Option.empty,
        arrayFilters = Seq.empty
      ).map(
        x =>
          x.result(indexKeyReads)
            .getOrElse(throw new Exception(s"Unable to generate ArrivalId")))
    )
  }
}

object ArrivalIdRepository {

  val collectionName = "arrival-ids"
}
