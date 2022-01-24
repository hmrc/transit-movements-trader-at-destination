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

import akka.Done
import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.all._
import com.google.inject.Inject
import com.kenshoo.play.metrics.Metrics
import config.AppConfig
import logging.Logging
import metrics.HasMetrics
import models._
import models.response.ResponseArrival
import models.response.ResponseArrivals
import play.api.Configuration
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.akkastream.cursorProducer
import reactivemongo.api.Cursor
import reactivemongo.api.bson.BSONDocument
import reactivemongo.api.bson.collection.BSONSerializationPack
import reactivemongo.api.indexes.Index.Aux
import reactivemongo.api.indexes.IndexType
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection
import utils.IndexUtils

import java.time.Clock
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ArrivalMovementRepository @Inject()(
  mongo: ReactiveMongoApi,
  appConfig: AppConfig,
  config: Configuration,
  clock: Clock,
  val metrics: Metrics
)(implicit ec: ExecutionContext, m: Materializer)
    extends Repository
    with MongoDateTimeFormats
    with Logging
    with HasMetrics {

  val started: Future[Unit] =
    collection
      .flatMap {
        jsonCollection =>
          for {
            _   <- dropLastUpdatedIndex(jsonCollection)
            _   <- jsonCollection.indexesManager.ensure(lastUpdatedIndex)
            _   <- jsonCollection.indexesManager.ensure(eoriNumberIndex)
            _   <- jsonCollection.indexesManager.ensure(channelIndex)
            _   <- jsonCollection.indexesManager.ensure(fetchAllIndex)
            _   <- jsonCollection.indexesManager.ensure(fetchAllWithDateFilterIndex)
            res <- jsonCollection.indexesManager.ensure(movementReferenceNumber)
          } yield res
      }
      .map(
        _ => ()
      )

  private lazy val channelKey: (String, IndexType)                      = "channel"                 -> IndexType.Ascending
  private lazy val eoriKey: (String, IndexType)                         = "eoriNumber"              -> IndexType.Ascending
  private lazy val mrnKey: (String, IndexType)                          = "movementReferenceNumber" -> IndexType.Ascending
  private def lastUpdatedKey(indexType: IndexType): (String, IndexType) = "lastUpdated"             -> indexType

  private lazy val eoriNumberIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq(eoriKey),
    name = Some("eori-number-index")
  )

  private lazy val movementReferenceNumber: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq(mrnKey),
    name = Some("movement-reference-number-index")
  )

  private lazy val lastUpdatedIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq(lastUpdatedKey(IndexType.Ascending)),
    name = Some("last-updated-index"),
    options = BSONDocument("expireAfterSeconds" -> appConfig.cacheTtl)
  )

  private lazy val channelIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq(channelKey),
    name = Some("channel-index")
  )

  private lazy val fetchAllIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq(channelKey, eoriKey),
    name = Some("fetch-all-index")
  )

  private lazy val fetchAllWithDateFilterIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq(channelKey, eoriKey, lastUpdatedKey(IndexType.Descending)),
    name = Some("fetch-all-with-date-filter-index")
  )

  private lazy val oldLastUpdatedIndexName = "last-updated-index-6m"
  private lazy val collectionName          = ArrivalMovementRepository.collectionName

  def bulkInsert(arrivals: Seq[Arrival]): Future[Unit] =
    collection.flatMap {
      _.insert(ordered = false)
        .many(arrivals.map(Json.toJsObject[Arrival]))
        .map(
          _ => ()
        )
    }

  def insert(arrival: Arrival): Future[Unit] =
    collection.flatMap {
      _.insert(false)
        .one(Json.toJsObject(arrival))
        .map(
          _ => ()
        )
    }

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  def getMaxArrivalId: Future[Option[ArrivalId]] =
    if (featureFlag) {
      collection.flatMap(
        _.find(Json.obj(), None)
          .sort(Json.obj("_id" -> -1))
          .one[Arrival]
          .map(_.map(_.arrivalId))
      )
    } else Future.successful(None)

  def get(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[Arrival]] = {

    val selector = Json.obj(
      "_id"     -> arrivalId,
      "channel" -> channelFilter
    )

    withMetricsTimerAsync("mongo-get-arrival-by-id") {
      _ =>
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

    withMetricsTimerAsync("mongo-get-arrival-by-id") {
      _ =>
        collection.flatMap {
          _.find(selector, None)
            .one[Arrival]
        }
    }
  }

  def getWithoutMessages(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[ArrivalWithoutMessages]] = {
    val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

    val projection = ArrivalWithoutMessages.projection ++ nextMessageId

    collection
      .flatMap {
        c =>
          c.aggregateWith[ArrivalWithoutMessages](allowDiskUse = true) {
              _ =>
                import c.aggregationFramework._

                val initialFilter: PipelineOperator =
                  Match(Json.obj("_id" -> arrivalId, "channel" -> channelFilter))

                val transformations = List[PipelineOperator](Project(projection))
                (initialFilter, transformations)

            }
            .headOption

      }
      .map(
        opt =>
          opt.map(
            a => a.copy(nextMessageId = MessageId(a.nextMessageId.value + 1))
        )
      )
  }

  def getWithoutMessages(arrivalId: ArrivalId): Future[Option[ArrivalWithoutMessages]] = {
    val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

    val projection = ArrivalWithoutMessages.projection ++ nextMessageId

    collection
      .flatMap {
        c =>
          c.aggregateWith[ArrivalWithoutMessages](allowDiskUse = true) {
              _ =>
                import c.aggregationFramework._

                val initialFilter: PipelineOperator =
                  Match(Json.obj("_id" -> arrivalId))

                val transformations = List[PipelineOperator](Project(projection))
                (initialFilter, transformations)

            }
            .headOption

      }
      .map(
        opt =>
          opt.map(
            a => a.copy(nextMessageId = MessageId(a.nextMessageId.value + 1))
        )
      )
  }

  def getMessage(arrivalId: ArrivalId, channelFilter: ChannelType, messageId: MessageId): Future[Option[MovementMessage]] =
    collection.flatMap {
      c =>
        c.aggregateWith[MovementMessage](allowDiskUse = true) {
            _ =>
              import c.aggregationFramework._

              val initialFilter: PipelineOperator =
                Match(
                  Json.obj("_id" -> arrivalId, "channel" -> channelFilter, "messages" -> Json.obj("$elemMatch" -> Json.obj("messageId" -> messageId.value))))

              val unwindMessages = List[PipelineOperator](
                Unwind(
                  path = "messages",
                  includeArrayIndex = None,
                  preserveNullAndEmptyArrays = None
                )
              )

              val secondaryFilter = List[PipelineOperator](Match(Json.obj("messages.messageId" -> messageId.value)))

              val groupById = List[PipelineOperator](GroupField("_id")("messages" -> FirstField("messages")))

              val replaceRoot = List[PipelineOperator](ReplaceRootField("messages"))

              val transformations = unwindMessages ++ secondaryFilter ++ groupById ++ replaceRoot

              (initialFilter, transformations)

          }
          .headOption
    }

  def getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType]): Future[Option[ArrivalMessages]] =
    collection.flatMap {
      c =>
        c.aggregateWith[ArrivalMessages](allowDiskUse = true) {
            _ =>
              import c.aggregationFramework._

              val initialFilter: PipelineOperator = Match(Json.obj("_id" -> arrivalId, "channel" -> channelFilter))

              val project = List[PipelineOperator](Project(Json.obj("_id" -> 1, "eoriNumber" -> 1, "messages" -> 1)))

              // Filters out the messages with message types we don't care about, meaning that if we have a
              // match for the arrival id and the eori number, but not a message type match,
              // we then get an empty list which indiates an arrival that doesn't have the required message
              // types as of yet, but would otherwise be valid
              val inFilter       = Json.obj("$in"     -> Json.arr("$$message.messageType", messageTypes.map(_.code).toSeq))
              val messagesFilter = Json.obj("$filter" -> Json.obj("input" -> "$messages", "as" -> "message", "cond" -> inFilter))

              val secondaryFilter = List[PipelineOperator](AddFields(Json.obj("messages" -> messagesFilter)))

              val transformations = project ++ secondaryFilter

              (initialFilter, transformations)

          }
          .headOption
    }

  def fetchAllArrivals(
    enrolmentId: Ior[TURN, EORINumber],
    channelFilter: ChannelType,
    updatedSince: Option[OffsetDateTime],
    movementReference: Option[String] = None,
    pageSize: Option[Int] = None,
    page: Option[Int] = None
  ): Future[ResponseArrivals] =
    withMetricsTimerAsync("mongo-get-arrivals-for-eori") {
      _ =>
        val enrolmentIds = enrolmentId.fold(
          turn => List(turn.value),
          eoriNumber => List(eoriNumber.value),
          (turn, eoriNumber) => List(eoriNumber.value, turn.value)
        )

        val baseSelector = Json.obj(
          "eoriNumber" -> Json.obj("$in" -> enrolmentIds),
          "channel"    -> channelFilter
        )

        val dateSelector = updatedSince
          .map {
            dateTime =>
              Json.obj("lastUpdated" -> Json.obj("$gte" -> dateTime))
          }
          .getOrElse {
            Json.obj()
          }

        val mrnSelector = movementReference
          .map {
            mrn =>
              Json.obj("movementReferenceNumber" -> Json.obj("$regex" -> mrn, "$options" -> "i"))
          }
          .getOrElse {
            Json.obj()
          }

        val fullSelector =
          baseSelector ++ dateSelector ++ mrnSelector

        val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

        val projection = ArrivalWithoutMessages.projection ++ nextMessageId

        val limit = pageSize.map(Math.max(1, _)).getOrElse(appConfig.maxRowsReturned(channelFilter))

        val skip = Math.abs(page.getOrElse(1) - 1) * limit

        collection.flatMap {
          coll =>
            val fetchCount      = coll.simpleCount(baseSelector)
            val fetchMatchCount = coll.simpleCount(fullSelector)

            val fetchResults = coll
              .aggregateWith[ArrivalWithoutMessages](allowDiskUse = true) {
                _ =>
                  import coll.aggregationFramework._

                  val matchStage   = Match(fullSelector)
                  val projectStage = Project(projection)
                  val sortStage    = Sort(Descending("lastUpdated"))
                  val skipStage    = Skip(skip)
                  val limitStage   = Limit(limit)

                  val restStages =
                    if (skip > 0)
                      List[PipelineOperator](projectStage, sortStage, skipStage, limitStage)
                    else
                      List[PipelineOperator](projectStage, sortStage, limitStage)

                  (matchStage, restStages)
              }
              .collect[Seq](limit, Cursor.FailOnError())

            (fetchResults, fetchCount, fetchMatchCount).mapN {
              case (results, count, matchCount) =>
                ResponseArrivals(
                  results.map(ResponseArrival.build),
                  results.length,
                  totalArrivals = count,
                  totalMatched = matchCount
                )
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
                .map(
                  x => Failure(new Exception(x))
                )
                .getOrElse(Failure(new Exception("Unable to update message status")))
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
          "updated"     -> message.dateTime,
          "lastUpdated" -> LocalDateTime.now(clock)
        ),
        "$inc" -> Json.obj(
          "nextMessageCorrelationId" -> 1
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.simpleFindAndUpdate(
        selector = selector,
        update = modifier
      ).map {
        _.lastError
          .map {
            le =>
              if (le.updatedExisting) Success(()) else Failure(new Exception(s"Could not find arrival $arrivalId"))
          }
          .getOrElse(Failure(new Exception("Failed to update arrival")))
      }
    }
  }

  def addResponseMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]] = {
    val selector = Json.obj(
      "_id" -> arrivalId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated"     -> message.dateTime,
          "lastUpdated" -> LocalDateTime.now(clock)
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.simpleFindAndUpdate(
        selector = selector,
        update = modifier
      ).map {
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
          .mapMaterializedValue(
            _.map(
              _ => Done
            )
          )
      }
  }

  def arrivalsWithoutJsonMessages(limit: Int): Future[Seq[Arrival]] =
    arrivalsWithoutJsonMessagesSource(limit).flatMap(
      _.runWith(Sink.seq[Arrival])
        .map {
          x =>
            logger.info(s"Found ${x.size} arrivals without JSON to process"); x
        }
    )

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

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
      _.simpleFindAndUpdate(
        selector = selector,
        update = modifier
      ).map {
        _ =>
          true // TODO: Handle problems?
      }
    }
  }

  private def dropLastUpdatedIndex(collection: JSONCollection): Future[Boolean] =
    collection.indexesManager.list.flatMap {
      indexes =>
        if (indexes.exists(_.name.contains(oldLastUpdatedIndexName))) {

          logger.warn(s"Dropping $oldLastUpdatedIndexName index")

          collection.indexesManager
            .drop(oldLastUpdatedIndexName)
            .map(
              _ => true
            )
        } else {
          logger.info(s"$oldLastUpdatedIndexName does not exist or has already been dropped")
          Future.successful(true)
        }
    }

}

object ArrivalMovementRepository {
  val collectionName = "arrival-movements"
}

case class SubmittedArrivalSummary(
  _id: ArrivalId,
  movementReferenceNumber: MovementReferenceNumber,
  eoriNumber: String,
  nextMessageCorrelationId: Int,
  lastUpdated: LocalDateTime,
  created: LocalDateTime
) {

  val obfuscatedEori: String               = s"ending ${eoriNumber.takeRight(4)}"
  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  val logMessage: String =
    s"""Movement in ArrivalSubmitted status
       |  Arrival Id: ${_id.index}
       |  MRN: ${movementReferenceNumber.value}
       |  EORI: $obfuscatedEori
       |  Last updated: ${dateTimeFormatter.format(lastUpdated)}
       |  Created: ${dateTimeFormatter.format(created)}
       |  Next message correlation Id: $nextMessageCorrelationId
       |""".stripMargin
}

object SubmittedArrivalSummary extends MongoDateTimeFormats {

  implicit val format: OFormat[SubmittedArrivalSummary] =
    Json.format[SubmittedArrivalSummary]
}
