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
import models.Arrival
import models.ArrivalMovement
import models.State
import models.request.ArrivalId
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.Cursor
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ArrivalMovementRepository @Inject()(cc: ControllerComponents, mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) {

  private val index = Index(
    key = Seq("eoriNumber" -> IndexType.Ascending),
    name = Some("eori-number-index")
  )

  //TODO: investigate if this needs to be done at app start
  val started: Future[Unit] = {
    collection
      .flatMap {
        _.indexesManager.ensure(index)
      }
      .map(_ => ())
  }

  private val collectionName = CollectionNames.ArrivalMovementCollection

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def insert(arrival: Arrival): Future[Unit] = {

    val mongoId = Json.obj("_id" -> arrival.arrivalId)

    collection.flatMap {
      _.insert(false)
        .one(Json.toJsObject(arrival) ++ mongoId)
        .map(_ => ())
    }
  }

  @deprecated
  def persistToMongo(arrivalMovement: ArrivalMovement): Future[WriteResult] = {

    val doc: JsObject = Json.toJson(arrivalMovement).as[JsObject]

    collection.flatMap {
      _.insert(false)
        .one(doc)
    }
  }

  def fetchAllMovements(eoriNumber: String): Future[Seq[ArrivalMovement]] =
    collection.flatMap {
      _.find(Json.obj("eoriNumber" -> eoriNumber), Option.empty[JsObject])
        .cursor[ArrivalMovement]()
        .collect[Seq](-1, Cursor.FailOnError())
    }

  def setState(id: ArrivalId, state: State): Future[Unit] = {

    val selector = Json.obj(
      "_id" -> id
    )

    val modifier = Json.obj(
      "$set" -> Json.obj(
        "state" -> state.toString
      )
    )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map(_ => ())
    }
  }
}

sealed trait FailedSavingArrivalMovement

object FailedSavingArrivalNotification extends FailedSavingArrivalMovement
