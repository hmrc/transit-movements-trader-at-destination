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

import java.time.LocalDateTime

import com.google.inject.Inject
import models.Arrival
import models.ArrivalId
import models.ArrivalModifier
import models.ArrivalStatus
import models.ArrivalUpdate
import models.MessageId
import models.MessageStatus
import models.MessageStatusUpdate
import models.MongoDateTimeFormats
import models.MovementMessage
import models.MovementReferenceNumber
import play.api.Configuration
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.Cursor
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType
import reactivemongo.bson.BSONDocument
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ArrivalMovementRepository @Inject()(mongo: ReactiveMongoApi, config: Configuration)(implicit ec: ExecutionContext) extends MongoDateTimeFormats {

  private val index = Index(
    key = Seq("eoriNumber" -> IndexType.Ascending),
    name = Some("eori-number-index")
  )

  private val cacheTtl = config.get[Int]("mongodb.timeToLiveInSeconds")

  private val lastUpdatedIndex = Index(
    key = Seq("lastUpdated" -> IndexType.Ascending),
    name = Some("last-updated-index"),
    options = BSONDocument("expireAfterSeconds" -> cacheTtl)
  )

  val started: Future[Unit] = {
    collection
      .flatMap {
        jsonCollection =>
          for {
            _   <- jsonCollection.indexesManager.ensure(index)
            res <- jsonCollection.indexesManager.ensure(lastUpdatedIndex)
          } yield res
      }
      .map(_ => ())
  }

  private val collectionName = ArrivalMovementRepository.collectionName

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def insert(arrival: Arrival): Future[Unit] = {
    val updatedArrival = arrival copy (lastUpdated = LocalDateTime.now)

    collection.flatMap {
      _.insert(false)
        .one(Json.toJsObject(updatedArrival))
        .map(_ => ())
    }
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

  def updateArrival[A](selector: JsObject, modifier: A)(implicit ev: ArrivalModifier[A]): Future[Try[Unit]] = {

    import models.ArrivalModifier.toJson

    collection.flatMap {
      _.update(false)
        .one[JsObject, JsObject](selector, modifier)
        .map {
          writeResult =>
            if (writeResult.n > 0)
              Success(())
            else
              writeResult.errmsg
                .map(x => Failure(new Exception(x)))
                .getOrElse(Failure(new Exception("Unable to update message status")))
        }
    }
  }

  @deprecated("Use updateArrival since this will be removed in the next version", "next")
  def setMessageState(arrivalId: ArrivalId, messageId: Int, status: MessageStatus): Future[Try[Unit]] = {
    val selector = Json.obj(
      "$and" -> Json.arr(
        Json.obj("_id"                         -> arrivalId),
        Json.obj(s"messages.$messageId.status" -> Json.obj("$exists" -> true))
      )
    )

    val modifier = ArrivalUpdate(None, Some(MessageStatusUpdate(MessageId.fromIndex(messageId), status)))

    updateArrival(selector, modifier)
  }

  @deprecated("Use updateArrival since this will be removed in the next version", "next")
  def setArrivalStateAndMessageState(arrivalId: ArrivalId,
                                     messageId: MessageId,
                                     arrivalState: ArrivalStatus,
                                     messageState: MessageStatus): Future[Option[Unit]] = {

    val selector = Json.obj("_id" -> arrivalId)

    val modifier = ArrivalUpdate(Some(arrivalState), Some(MessageStatusUpdate(messageId, messageState)))

    updateArrival(selector, modifier).map(_.toOption)
  }

  def addNewMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated"    -> message.dateTime,
          "lastUpdate" -> LocalDateTime.now
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

  def addResponseMessage(arrivalId: ArrivalId, message: MovementMessage, status: ArrivalStatus): Future[Try[Unit]] = {
    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated"     -> message.dateTime,
          "lastUpdated" -> LocalDateTime.now,
          "status"      -> status.toString
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
