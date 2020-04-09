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
import models.MongoDateTimeFormats
import models.{Arrival, ArrivalId, MessageState, MovementMessage, MovementReferenceNumber, State}
import models.MovementMessageWithState
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.Cursor
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ArrivalMovementRepository @Inject()(cc: ControllerComponents, mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) extends MongoDateTimeFormats {

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

  def setMessageState(arrivalId: ArrivalId, messageId: Int, state: MessageState): Future[Try[Unit]] = {

    //      s"messages.state" ->
    //        Json.obj("$elemMatch" ->
    //          Json.obj("state" -> Json.obj("$exists" -> true))))

//    val selector = Json.obj(
//      "_id" -> arrivalId,
//      s"messages.$messageId" -> Json.obj("$elemMatch" ->
//              Json.obj("state" -> Json.obj("$exists" -> true)))

//    {$and: [{"_id": 123}, {"messages.0.state": { "$exists": true}}]}
    val selector = Json.obj(
      "$and" ->
        Json.arr(Json.obj("_id" -> arrivalId),
                 Json.obj(s"messages.$messageId.state" ->
                   Json.obj("$exists" -> true))))

    val modifier = Json.obj(
      "$set" -> Json.obj(
        s"messages.$messageId.state" -> state.toString
      )
    )
    println(state.toString)

    collection.flatMap {
      x =>
        val builder = x.update(false)
          builder.one(selector, modifier).map {
          x =>
            {
              println(x.code)
              println(x.nModified)
              println(x.writeErrors)
              println(x.ok)
              println(x.errmsg)
              Success(())
            }
        }
    }

//    collection.flatMap {
//      _.findAndUpdate(selector, modifier)
//        .map(_ => Success(()))

    //        .map {
//          _.lastError
//            .map {
//              le =>
//                if (le.updatedExisting) Success(())
//                else
//                  Failure(new Exception(le.err match {
//                    case Some(err) => err
//                    case None      => "Unknown Error"
//                  }))
//            }
//            .getOrElse(Failure(new Exception("Failed to update arrival")))
//        }
//    }
  }

  // TODO: Set return type to Future[Try[Unit]] ?
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

  //TODO: Return index position of the new message.
  def addMessage(arrivalId: ArrivalId, message: MovementMessage, state: State): Future[Try[Unit]] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier = Json.obj(
      "$set" -> Json.obj(
        "state" -> state.toString
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
