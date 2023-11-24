/*
 * Copyright 2023 HM Revenue & Customs
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
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.kenshoo.play.metrics.Metrics
import com.mongodb.client.model.Filters.empty
import com.mongodb.client.model.Updates
import config.AppConfig
import logging.Logging
import metrics.HasMetrics
import models.Arrival
import models.ArrivalId
import models.ArrivalMessages
import models.ArrivalModifier
import models.ArrivalSelector
import models.ArrivalWithoutMessages
import models.Box
import models.ChannelType
import models.EORINumber
import models.MessageId
import models.MessageStatusUpdate
import models.MessageType
import models.MongoDateTimeFormats
import models.MovementMessage
import models.MovementReferenceNumber
import models.TURN
import models.response.ResponseArrival
import models.response.ResponseArrivals
import org.bson.conversions.Bson
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.Aggregates
import org.mongodb.scala.model.BulkWriteOptions
import org.mongodb.scala.model.Field
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.IndexModel
import org.mongodb.scala.model.IndexOptions
import org.mongodb.scala.model.Indexes
import org.mongodb.scala.model.InsertOneModel
import org.mongodb.scala.model.UpdateOptions
import org.mongodb.scala.model.Sorts.descending
import play.api.Configuration
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import repositories.ArrivalMovementRepositoryImpl.EPOCH_TIME
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import utils.IndexUtils

import java.time.Clock
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.Collections
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching.Regex

@ImplementedBy(classOf[ArrivalMovementRepositoryImpl])
trait ArrivalMovementRepository {
  val started: Future[Unit]
  def bulkInsert(arrivals: Seq[Arrival]): Future[Unit]
  def insert(arrival: Arrival): Future[Unit]
  def getMaxArrivalId: Future[Option[ArrivalId]]
  def get(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[Arrival]]
  def get(arrivalId: ArrivalId): Future[Option[Arrival]]
  def getWithoutMessages(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[ArrivalWithoutMessages]]
  def getWithoutMessages(arrivalId: ArrivalId): Future[Option[ArrivalWithoutMessages]]
  def getMessage(arrivalId: ArrivalId, channelFilter: ChannelType, messageId: MessageId): Future[Option[MovementMessage]]
  def getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType]): Future[Option[ArrivalMessages]]

  def fetchAllArrivals(
    enrolmentId: Ior[TURN, EORINumber],
    channelFilter: ChannelType,
    updatedSince: Option[OffsetDateTime],
    movementReference: Option[String] = None,
    pageSize: Option[Int] = None,
    page: Option[Int] = None
  ): Future[ResponseArrivals]
  def updateArrival(selector: ArrivalSelector, modifier: MessageStatusUpdate): Future[Try[Unit]]
  def addNewMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]]
  def addResponseMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]]
  def arrivalsWithoutJsonMessagesSource(limit: Int): Future[Source[Arrival, Future[Done]]]
  def arrivalsWithoutJsonMessages(limit: Int): Future[Seq[Arrival]]
  def resetMessages(arrivalId: ArrivalId, messages: NonEmptyList[MovementMessage]): Future[Boolean]
}

object ArrivalMovementRepositoryImpl {
  val EPOCH_TIME: LocalDateTime = LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)
}

//@Singleton
class ArrivalMovementRepositoryImpl @Inject()(
  mongo: MongoComponent,
  appConfig: AppConfig,
  config: Configuration,
  clock: Clock,
  val metrics: Metrics
)(implicit ec: ExecutionContext, m: Materializer)
    extends PlayMongoRepository[Arrival](
      mongoComponent = mongo,
      collectionName = "arrival-movements-hmrc-mongo",
      domainFormat = Arrival.formatsArrival,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("lastUpdated"),
          IndexOptions()
            .name("lastUpdatedIndex")
            .expireAfter(appConfig.cacheTtl, TimeUnit.SECONDS)
            .background(true)
        ),
        IndexModel(
          Indexes.ascending("eoriNumber"),
          IndexOptions()
            .name("eori-number-index")
            .background(true)
        ),
        IndexModel(
          Indexes.ascending("channel"),
          IndexOptions()
            .name("channel-index")
            .background(true)
        ),
        IndexModel(
          Indexes.ascending("channel", "eoriNumber"),
          IndexOptions()
            .name("fetch-all-index")
            .background(true)
        ),
        IndexModel(
          Indexes.ascending("channel", "eoriNumber"),
          IndexOptions()
            .name("fetch-all-with-date-filter-index")
            .background(true)
        ),
        IndexModel(
          Indexes.compoundIndex(
            Indexes.ascending("channel"),
            Indexes.ascending("eoriNumber"),
            Indexes.descending("lastUpdated")
          )
        ),
        IndexModel(
          Indexes.ascending("movementReferenceNumber"),
          IndexOptions()
            .name("movement-reference-number-index")
            .background(true)
        )
      ),
      extraCodecs = Seq(
        Codecs.playFormatCodec(ArrivalId.formatsArrivalId),
//        TODO: finish,
        Codecs.playFormatCodec(ChannelType.formats),
        Codecs.playFormatCodec(MovementReferenceNumber.mrnFormat),
        Codecs.playFormatCodec(Box.formatsBox)
      )
    )
    with ArrivalMovementRepository
    with MongoDateTimeFormats
    with Logging
    with HasMetrics {

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  val started: Future[Unit] = ensureIndexes.map(
    _ => ()
  )

  def bulkInsert(arrivals: Seq[Arrival]): Future[Unit] = {
    val insertModels = arrivals.map(
      arrival => InsertOneModel(arrival)
    )
    collection
      .bulkWrite(insertModels, BulkWriteOptions().ordered(false))
      .toFuture()
      .map(
        _ => ()
      )
  }

  def insert(arrival: Arrival): Future[Unit] =
    collection
      .insertOne(arrival)
      .toFuture()
      .map(
        _ => ()
      )

  def getMaxArrivalId: Future[Option[ArrivalId]] =
    if (featureFlag) {
      collection.find().sort(descending("_id")).limit(1).headOption().map(_.map(_.arrivalId))
    } else Future.successful(None)

  def get(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[Arrival]] =
    withMetricsTimerAsync("mongo-get-arrival-by-id") {
      _ =>
        collection
          .find(Filters.and(Filters.eq("_id", arrivalId), Filters.eq("channel", channelFilter.toString)))
          .headOption()
    }

  def get(arrivalId: ArrivalId): Future[Option[Arrival]] =
    withMetricsTimerAsync("mongo-get-arrival-by-id") {
      _ =>
        collection
          .find(Filters.eq("_id", arrivalId))
          .headOption()
    }

  def getWithoutMessages(arrivalId: ArrivalId, channelFilter: ChannelType): Future[Option[ArrivalWithoutMessages]] = {
    val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

    val projection       = ArrivalWithoutMessages.projection ++ nextMessageId
    val filter           = Filters.and(Filters.eq("_id", arrivalId), Filters.eq("channel", channelFilter.toString))
    val aggregatesFilter = Aggregates.filter(filter)
    val aggregates       = Seq(aggregatesFilter, Aggregates.project(Codecs.toBson(projection).asDocument()))

    collection
      .aggregate[ArrivalWithoutMessages](aggregates)
      .allowDiskUse(true)
      .headOption()
      .map(
        opt =>
          opt.map(
            a => a.copy(nextMessageId = MessageId(a.nextMessageId.value + 1))
        )
      )
  }

  def getWithoutMessages(arrivalId: ArrivalId): Future[Option[ArrivalWithoutMessages]] = {
    val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))
    val projection    = ArrivalWithoutMessages.projection ++ nextMessageId
    val filter        = Aggregates.filter(Filters.eq("_id", arrivalId))
    val aggregates    = Seq(filter, Aggregates.project(Codecs.toBson(projection).asDocument()))
    collection
      .aggregate[ArrivalWithoutMessages](aggregates)
      .allowDiskUse(true)
      .headOption()
      .map(
        opt =>
          opt.map(
            d => d.copy(nextMessageId = MessageId(d.nextMessageId.value + 1))
        )
      )
  }

  def getMessage(arrivalId: ArrivalId, channelFilter: ChannelType, messageId: MessageId): Future[Option[MovementMessage]] = {
    val initialFilter = Aggregates.filter(
      Filters.and(
        Filters.eq("_id", arrivalId),
        Filters.eq("channel", channelFilter.toString),
        Filters.elemMatch("messages", Filters.eq("messageId", messageId.value))
      )
    )
    val unwindMessages  = Aggregates.unwind("$messages")
    val secondaryFilter = Aggregates.filter(Filters.eq("messages.messageId", messageId.value))
    val replaceRoot     = Aggregates.replaceRoot("$messages")
    val aggregates      = Seq(initialFilter, unwindMessages, secondaryFilter, replaceRoot)
    collection.aggregate[MovementMessage](aggregates).allowDiskUse(true).headOption()
  }

  def getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType]): Future[Option[ArrivalMessages]] = {
    val filter  = Aggregates.filter(Filters.and(Filters.eq("_id", arrivalId), Filters.eq("channel", channelFilter.toString)))
    val project = Aggregates.project(Codecs.toBson(ArrivalMessages.projection).asDocument())
    val messagesFilter = BsonDocument(
      "$filter" -> Document(
        "input" -> "$messages",
        "as"    -> "message",
        "cond"  -> Document("$in" -> BsonArray("$$message.messageType", messageTypes.map(_.code).toSeq))
      )
    )
    val secondaryFilter = Aggregates.addFields(Field("messages", messagesFilter))

    val aggregates = Seq(filter, project, secondaryFilter)
    collection.aggregate[ArrivalMessages](aggregates).allowDiskUse(true).headOption()
  }

  private def mrnFilter(movementReferenceNumber: Option[String]): Bson =
    movementReferenceNumber match {
      case Some(movementReferenceNumber) => Filters.regex("movementReferenceNumber", s"\\Q$movementReferenceNumber\\E", "i")
      case _                             => empty()
    }

  def fetchAllArrivals(
    enrolmentId: Ior[TURN, EORINumber],
    channelFilter: ChannelType,
    updatedSince: Option[OffsetDateTime],
    movementReference: Option[String] = None,
    pageSize: Option[Int] = None,
    page: Option[Int] = None
  ): Future[ResponseArrivals] = withMetricsTimerAsync("mongo-get-arrivals-for-eori") {
    _ =>
      val enrolmentIds = enrolmentId.fold(
        turn => List(turn.value),
        eoriNumber => List(eoriNumber.value),
        (turn, eoriNumber) => List(eoriNumber.value, turn.value)
      )

      val baseSelector = Filters.and(Filters.in("eoriNumber", enrolmentIds.map(_.toString): _*), Filters.eq("channel", channelFilter.toString))

      val dateTimeFilter: Bson =
        Filters.gte("lastUpdated", updatedSince.map(_.toLocalDateTime).getOrElse(EPOCH_TIME))
      val fullSelector =
        Filters.and(
          Filters.in("eoriNumber", enrolmentIds.map(_.toString): _*),
          Filters.eq("channel", channelFilter.toString),
          mrnFilter(movementReference),
          dateTimeFilter
        )

      val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

      val projection = ArrivalWithoutMessages.projection ++ nextMessageId
      val limit      = pageSize.map(Math.max(1, _)).getOrElse(appConfig.maxRowsReturned(channelFilter))

      val skip            = Math.abs(page.getOrElse(1) - 1) * limit
      val fetchCount      = collection.countDocuments(baseSelector).toFuture().map(_.toInt)
      val fetchMatchCount = collection.countDocuments(fullSelector).toFuture().map(_.toInt)
      val matchStage      = Aggregates.filter(fullSelector)
      val projectStage    = Aggregates.project(Codecs.toBson(projection).asDocument())
      val sortStage       = Aggregates.sort(descending("lastUpdated"))
      val skipStage       = Aggregates.skip(skip)
      val limitStage      = Aggregates.limit(limit)
      val restStages =
        if (skip > 0)
          Seq(matchStage) ++ Seq(projectStage, sortStage, skipStage, limitStage)
        else
          Seq(matchStage) ++ Seq(projectStage, sortStage, limitStage)
      val fetchResults = collection
        .aggregate[ArrivalWithoutMessages](restStages)
        .allowDiskUse(true)
        .toFuture()
        .map {
          response =>
            response.map(ResponseArrival.build(_))
        }

      for {
        fetchResults    <- fetchResults
        fetchCount      <- fetchCount
        fetchMatchCount <- fetchMatchCount
      } yield ResponseArrivals(fetchResults, fetchResults.length, fetchCount, fetchMatchCount)
  }

  def updateArrival(selector: ArrivalSelector, modifier: MessageStatusUpdate): Future[Try[Unit]] = {
    val filter     = Filters.eq("_id", selector)
    val setUpdated = Updates.set("lastUpdated", LocalDateTime.now(clock))

    val arrayFilters = new UpdateOptions().arrayFilters(Collections.singletonList(Filters.in("element.messageId", modifier.messageId.value)))
    val setStatus    = Updates.set("messages.$[element].status", modifier.messageStatus.toString)
    collection.updateOne(filter = filter, update = Updates.combine(setStatus, setUpdated), options = arrayFilters).toFuture().map {
      result =>
        if (result.wasAcknowledged()) {
          if (result.getModifiedCount == 0) Failure(new Exception("Unable to update message status"))
          else Success(())
        } else Failure(new Exception("Unable to update message status"))

    }
  }

  def addNewMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]] =
    collection
      .updateOne(
        filter = Filters.eq("_id", arrivalId),
        update = Updates.combine(
          Updates.set("updated", message.received.get),
          Updates.set("lastUpdated", message.received.get),
          Updates.inc("nextMessageCorrelationId", 1),
          Updates.push("messages", message)
        )
      )
      .toFuture()
      .map {
        result =>
          if (result.wasAcknowledged()) {
            if (result.getModifiedCount == 0) Failure(new Exception(s"Could not find arrival $arrivalId"))
            else Success(())
          } else Failure(new Exception("Failed to update arrival"))

      }

  def addResponseMessage(arrivalId: ArrivalId, message: MovementMessage): Future[Try[Unit]] = {
    val selector = Filters.eq("_id", arrivalId)
    val modifier = Updates.combine(
      Updates.set("updated", message.received.get),
      Updates.set("lastUpdated", message.received.get),
      Updates.push("messages", message)
    )
    collection
      .updateOne(filter = selector, update = modifier)
      .toFuture()
      .map {
        result =>
          if (result.wasAcknowledged()) {
            if (result.getModifiedCount == 0) Failure(new Exception(s"Could not find arrival $arrivalId"))
            else Success(())
          } else Failure(new Exception("Failed to update arrival"))

      }
  }

  def arrivalsWithoutJsonMessagesSource(limit: Int): Future[Source[Arrival, Future[Done]]] = ???
//{
//    val messagesWithNoJson =
//      Json.obj(
//        "messages" -> Json.obj(
//          "$elemMatch" -> Json.obj(
//            "messageJson" -> Json.obj(
//              "$exists" -> false
//            )
//          )
//        )
//      )
//
//    val messagesWithEmptyJson =
//      Json.obj(
//        "messages" -> Json.obj(
//          "$elemMatch" -> Json.obj(
//            "messageJson" -> Json.obj()
//          )
//        )
//      )
//
//    //    val query = Json.obj("$or" -> Json.arr(messagesWithNoJson, messagesWithEmptyJson))
//    val selector = Filters.or(Filters.equal("???", messagesWithNoJson), Filters.equal("???", messagesWithEmptyJson))
//    collection.find() // HERE
//
//    collection
//      .map {
//        _.find[JsObject, Arrival](query, None)
//          .cursor[Arrival]()
//          .documentSource(maxDocs = limit)
//          .mapMaterializedValue(
//            _.map(
//              _ => Done
//            )
//          )
//  }

  def arrivalsWithoutJsonMessages(limit: Int): Future[Seq[Arrival]] =
    arrivalsWithoutJsonMessagesSource(limit).flatMap(
      _.runWith(Sink.seq[Arrival])
        .map {
          x =>
            logger.info(s"Found ${x.size} arrivals without JSON to process"); x
        }
    )

  def resetMessages(arrivalId: ArrivalId, messages: NonEmptyList[MovementMessage]): Future[Boolean] = ???
//    val selector = Json.obj(
//      "_id" -> arrivalId
//    )
//
//    val modifier = Json.obj(
//      "$set" -> Json.obj(
//        "messages" -> Json.toJson(messages.toList)
//      )
//    )
//
//    collection.flatMap {
//      _.simpleFindAndUpdate(
//        selector = selector,
//        update = modifier
//      ).map {
//        _ =>
//          true // TODO: Handle problems?
//      }
//    }

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
