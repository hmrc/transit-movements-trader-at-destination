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

import com.google.inject.Inject
import models.ArrivalId
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

class ArrivalIdRepository @Inject() (mongo: ReactiveMongoApi, config: Configuration) extends Repository {

  private val lastIndexKey = "last-index"
  private val primaryValue = "record_id"

  private val collectionName: String = ArrivalIdRepository.collectionName

  private val indexKeyReads: Reads[ArrivalId] = {
    import play.api.libs.json._
    (__ \ lastIndexKey).read[ArrivalId]
  }

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def setNextId(nextId: Int): Future[Unit] =
    if (featureFlag) {
      val update = Json.obj(
        "$set" -> Json.obj(lastIndexKey -> nextId)
      )

      val selector = Json.obj("_id" -> primaryValue)

      collection.flatMap(
        _.update(ordered = false)
          .one(selector, update, upsert = true)
          .flatMap {
            result =>
              if (result.ok)
                Future.unit
              else
                result.errmsg
                  .map(
                    x => Future.failed(new Exception(x))
                  )
                  .getOrElse(Future.failed(new Exception("Unable to set next ArrivalId")))
          }
      )
    } else {
      Future.failed(new Exception("Feature disabled, cannot set next ArrivalId"))
    }

  def nextId(): Future[ArrivalId] = {

    val update = Json.obj(
      "$inc" -> Json.obj(lastIndexKey -> 1)
    )

    val selector = Json.obj("_id" -> primaryValue)

    collection.flatMap(
      _.simpleFindAndUpdate(
        selector = selector,
        update = update,
        fetchNewObject = true,
        upsert = true
      ).map(
        x =>
          x.result(indexKeyReads)
            .getOrElse(throw new Exception(s"Unable to generate ArrivalId"))
      )
    )
  }
}

object ArrivalIdRepository {

  val collectionName = "arrival-ids"
}
