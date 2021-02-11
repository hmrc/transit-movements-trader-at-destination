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

import akka.Done
import akka.NotUsed

import java.time.LocalDateTime
import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import cats.data.NonEmptyList
import com.google.inject.Inject
import config.AppConfig
import logging.Logging
import metrics.MetricsService
import metrics.Monitors
import models.Arrival
import models.ArrivalId
import models.ArrivalIdSelector
import models.ArrivalModifier
import models.ArrivalSelector
import models.ArrivalStatus
import models.ArrivalStatusUpdate
import models.ChannelType
import models.CompoundStatusUpdate
import models.MessageId
import models.MessageStatus
import models.MessageStatusUpdate
import models.MongoDateTimeFormats
import models.MovementMessage
import models.MovementReferenceNumber
import models.response.ResponseArrival
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.akkastream.cursorProducer
import reactivemongo.akkastream.State
import reactivemongo.api.Cursor
import reactivemongo.api.bson.collection.BSONSerializationPack
import reactivemongo.api.indexes.Index.Aux
import reactivemongo.api.indexes.IndexType
import reactivemongo.bson.BSONDocument
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.IndexUtils

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ArrivalMovementRepository @Inject()(
  mongo: ReactiveMongoApi,
  appConfig: AppConfig,
  metricsService: MetricsService
)(implicit ec: ExecutionContext, m: Materializer)
    extends MongoDateTimeFormats
    with Logging {

  private val eoriNumberIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("eoriNumber" -> IndexType.Ascending),
    name = Some("eori-number-index")
  )

  private val movementReferenceNumber: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("movementReferenceNumber" -> IndexType.Ascending),
    name = Some("movement-reference-number-index")
  )

  private val lastUpdatedIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("lastUpdated" -> IndexType.Ascending),
    name = Some("last-updated-index"),
    options = BSONDocument("expireAfterSeconds" -> appConfig.cacheTtl)
  )

  val started: Future[Unit] = {
    collection
      .flatMap {
        jsonCollection =>
          for {
            _   <- jsonCollection.indexesManager.ensure(eoriNumberIndex)
            _   <- jsonCollection.indexesManager.ensure(movementReferenceNumber)
            res <- jsonCollection.indexesManager.ensure(lastUpdatedIndex)
          } yield res
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

  def get(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[Arrival]] = {

    val selector = Json.obj(
      "_id"     -> arrivalId,
      "channel" -> channelFilter
    )

    metricsService.timeAsyncCall(Monitors.GetArrivalByIdMonitor) {
      collection.flatMap {
        _.find(selector, None)
          .one[Arrival]
      }
    }
  }

  def get(arrivalId: ArrivalId): Future[Option[Arrival]] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    metricsService.timeAsyncCall(Monitors.GetArrivalByIdMonitor) {
      collection.flatMap {
        _.find(selector, None)
          .one[Arrival]
      }
    }
  }

  def get(eoriNumber: String, mrn: MovementReferenceNumber, channelFilter: ChannelType): Future[Option[Arrival]] = {
    val selector = Json.obj(
      "movementReferenceNumber" -> mrn.value,
      "eoriNumber"              -> eoriNumber,
      "channel"                 -> channelFilter
    )

    metricsService.timeAsyncCall(Monitors.GetArrivalByMrnMonitor) {
      collection.flatMap {
        _.find(selector, None)
          .one[Arrival]
      }
    }
  }

  def fetchAllArrivals(eoriNumber: String, channelFilter: ChannelType): Future[Seq[ResponseArrival]] =
    metricsService.timeAsyncCall(Monitors.GetArrivalsForEoriMonitor) {
      collection.flatMap {
        _.find(Json.obj("eoriNumber" -> eoriNumber, "channel" -> channelFilter), Some(ResponseArrival.projection))
          .cursor[ResponseArrival]()
          .collect[Seq](-1, Cursor.FailOnError())
          .map {
            arrivals =>
              metricsService.inc(Monitors.arrivalsPerEori(arrivals))
              arrivals
          }
      }
    }

  def updateArrival[A](selector: ArrivalSelector, modifier: A)(implicit ev: ArrivalModifier[A]): Future[Try[Unit]] = {

    import models.ArrivalModifier.toJson

    collection.flatMap {
      _.update(false)
        .one[JsObject, JsObject](Json.toJsObject(selector), modifier)
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
  def setArrivalStateAndMessageState(arrivalId: ArrivalId,
                                     messageId: MessageId,
                                     arrivalState: ArrivalStatus,
                                     messageState: MessageStatus): Future[Option[Unit]] = {

    val selector = ArrivalIdSelector(arrivalId)

    val modifier = CompoundStatusUpdate(ArrivalStatusUpdate(arrivalState), MessageStatusUpdate(messageId, messageState))

    updateArrival(selector, modifier).map(_.toOption)
  }

  def addNewMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated"     -> message.dateTime,
          "lastUpdated" -> LocalDateTime.now
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

  def arrivalsWithoutJsonMessagesSource(limit: Int): Future[Source[Arrival, Future[Done]]] = {

    val messagesWithNoJson =
      Json.obj(
        "messages" -> Json.obj(
          "$elemMatch" -> Json.obj(
            "messageJson" -> Json.obj(
              "$exists" -> false
            )
          )
        )
      )

    val messagesWithEmptyJson =
      Json.obj(
        "messages" -> Json.obj(
          "$elemMatch" -> Json.obj(
            "messageJson" -> Json.obj()
          )
        )
      )

    val query = Json.obj("$or" -> Json.arr(messagesWithNoJson, messagesWithEmptyJson))

    collection
      .map {
        _.find[JsObject, Arrival](query, None)
          .cursor[Arrival]()
          .documentSource(maxDocs = limit)
          .mapMaterializedValue(_.map(_ => Done))
      }
  }

  def arrivalsWithoutJsonMessages(limit: Int): Future[Seq[Arrival]] =
    arrivalsWithoutJsonMessagesSource(limit).flatMap(
      _.runWith(Sink.seq[Arrival])
        .map(x => { logger.info(s"Found ${x.size} arrivals without JSON to process"); x })
    )

  def resetMessages(arrivalId: ArrivalId, messages: NonEmptyList[MovementMessage]): Future[Boolean] = {

    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier = Json.obj(
      "$set" -> Json.obj(
        "messages" -> Json.toJson(messages.toList)
      )
    )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map {
          _ =>
            true // TODO: Handle problems?
        }
    }
  }
}

object ArrivalMovementRepository {
  val collectionName = "arrival-movements"
}
