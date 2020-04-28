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
import models.ArrivalId
import models.ArrivalState
import models.MessageId
import models.MessageState
import models.MongoDateTimeFormats
import models.MovementMessage
import models.MovementReferenceNumber
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
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ArrivalMovementRepository @Inject()(mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) extends MongoDateTimeFormats {

  private val index = Index(
    key = Seq("eoriNumber" -> IndexType.Ascending),
    name = Some("eori-number-index")
  )

  val started: Future[Unit] = {
    collection
      .flatMap {
        _.indexesManager.ensure(index)
      }
      .map(_ => ())
  }

  private val collectionName = ArrivalMovementRepository.collectionName

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def insert(arrival: Arrival): Future[Unit] =
    collection.flatMap {
      _.insert(false)
        .one(Json.toJsObject(arrival))
        .map(_ => ())
    }

  def get(arrivalId: ArrivalId): Future[Option[Arrival]] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    collection.flatMap {
      _.find(selector, None)
        .one[Arrival]
    }
  }

  def get(eoriNumber: String, mrn: MovementReferenceNumber): Future[Option[Arrival]] = {
    val selector = Json.obj(
      "movementReferenceNumber" -> mrn.value,
      "eoriNumber"              -> eoriNumber
    )

    collection.flatMap {
      _.find(selector, None)
        .one[Arrival]
    }
  }

  def fetchAllArrivals(eoriNumber: String): Future[Seq[Arrival]] =
    collection.flatMap {
      _.find(Json.obj("eoriNumber" -> eoriNumber), Option.empty[JsObject])
        .cursor[Arrival]()
        .collect[Seq](-1, Cursor.FailOnError())
    }

  // TODO: Refactor this to take a MessageId
  def setMessageState(arrivalId: ArrivalId, messageId: Int, state: MessageState): Future[Try[Unit]] = {
    val selector = Json.obj(
      "$and" -> Json.arr(
        Json.obj("_id"                        -> arrivalId),
        Json.obj(s"messages.$messageId.state" -> Json.obj("$exists" -> true))
      )
    )

    val modifier = Json.obj(
      "$set" -> Json.obj(
        s"messages.$messageId.state" -> state.toString
      )
    )

    collection.flatMap {
      _.update(false)
        .one(selector, modifier)
        .map {
          WriteResult
            .lastError(_)
            .map {
              le =>
                if (le.updatedExisting) Success(())
                else
                  Failure(new Exception(le.errmsg match {
                    case Some(err) => err
                    case None      => "Unable to update message state"
                  }))
            }
            .getOrElse(Failure(new Exception("Unable to update message state")))
        }
    }
  }

  def setState(id: ArrivalId, state: ArrivalState): Future[Option[Unit]] = {

    val selector = Json.obj(
      "_id" -> id
    )

    val modifier = Json.obj(
      "$set" -> Json.obj(
        "state" -> state.toString
      )
    )

    collection.flatMap {
      _.update(false)
        .one(selector, modifier)
        .map {
          y =>
            if (y.n == 1) Some(())
            else None
        }
    }
  }

  def setArrivalStateAndMessageState(arrivalId: ArrivalId,
                                     messageId: MessageId,
                                     arrivalState: ArrivalState,
                                     messageState: MessageState): Future[Option[Unit]] = {

    val selector = Json.obj("_id" -> arrivalId)

    val modifier = Json.obj(
      "$set" -> Json.obj(
        s"messages.${messageId.index}.state" -> messageState.toString,
        "state"                              -> arrivalState.toString
      )
    )

    collection.flatMap {
      _.update(false)
        .one(selector, modifier)
        .map {
          y =>
            if (y.n == 1) Some(())
            else None
        }
    }
  }

  def addNewMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated" -> message.dateTime
        ),
        "$inc" -> Json.obj(
          "nextMessageCorrelationId" -> 1
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map {
          _.lastError
            .map {
              le =>
                if (le.updatedExisting) Success(()) else Failure(new Exception(s"Could not find arrival $arrivalId"))
            }
            .getOrElse(Failure(new Exception("Failed to update arrival")))
        }
    }
  }

  def addResponseMessage(arrivalId: ArrivalId, message: MovementMessage, state: ArrivalState): Future[Try[Unit]] = {
    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated" -> message.dateTime,
          "state"   -> state.toString
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map {
          _.lastError
            .map {
              le =>
                if (le.updatedExisting) Success(()) else Failure(new Exception(s"Could not find arrival $arrivalId"))
            }
            .getOrElse(Failure(new Exception("Failed to update arrival")))
        }
    }
  }
}

object ArrivalMovementRepository {
  val collectionName = "arrival-movements"
}
