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

package migrations

import java.time.LocalDateTime

import base.ItSpecBase
import cats.syntax.all._
import models.ArrivalId
import models.ChannelType
import models.MessageStatus
import models.MessageType
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.xml.NodeSeq

class MovementsChangeLogSpec extends ItSpecBase with IntegrationPatience with BeforeAndAfterAll with GuiceOneAppPerSuite {

  val arrivalIds = (1 to 20).map(ArrivalId.apply)
  val eori       = "123456789000"

  override protected def afterAll(): Unit = {
    val mongo = app.injector.instanceOf[ReactiveMongoApi]

    val dropDatabase = for {
      db <- mongo.database
      _  <- db.drop()
    } yield ()

    dropDatabase.futureValue
  }

  override def beforeAll(): Unit = {
    import models.MongoDateTimeFormats._
    import utils.NodeSeqFormat._

    val mongo = app.injector.instanceOf[ReactiveMongoApi]

    val insertArrivals = for {
      db <- mongo.database

      _ <- db.drop()

      coll = db.collection[JSONCollection](ArrivalMovementRepository.collectionName)

      _ <- coll.insert.many {
        for (id <- arrivalIds)
          yield Json.obj(
            "_id"                      -> id.index,
            "channel"                  -> ChannelType.web.toString,
            "eoriNumber"               -> eori,
            "movementReferenceNumber"  -> Random.alphanumeric.take(20).mkString,
            "created"                  -> LocalDateTime.of(2021, 7, 15, 12, 12),
            "updated"                  -> LocalDateTime.of(2021, 7, 15, 12, 13),
            "lastUpdated"              -> LocalDateTime.of(2021, 7, 15, 12, 13),
            "nextMessageCorrelationId" -> 2,
            "messages" -> Json.arr(
              Json.obj(
                "dateTime"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                "received"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                "messageType"          -> MessageType.ArrivalNotification.toString,
                "message"              -> NodeSeq.fromSeq(Seq(<CC007A></CC007A>)),
                "status"               -> MessageStatus.SubmissionSucceeded.toString,
                "messageCorrelationId" -> 1
              ),
              Json.obj(
                "dateTime"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                "received"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                "messageType"          -> MessageType.UnloadingPermission.toString,
                "message"              -> NodeSeq.fromSeq(Seq(<CC043A></CC043A>)),
                "messageCorrelationId" -> 1
              ),
              Json.obj(
                "dateTime"             -> LocalDateTime.of(2021, 7, 15, 12, 13),
                "received"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                "messageType"          -> MessageType.UnloadingRemarks.toString,
                "message"              -> NodeSeq.fromSeq(Seq(<CC044A></CC044A>)),
                "messageCorrelationId" -> 1
              )
            )
          )
      }
    } yield ()

    insertArrivals.futureValue
  }

  "MovementsChangeLog" - {
    "addMessageIdToMessages" - {
      "should add message ID to messages in the arrivals collection" in {
        val repo   = app.injector.instanceOf[ArrivalMovementRepository]
        val runner = app.injector.instanceOf[MigrationRunnerImpl]

        runner.runMigrations().futureValue

        arrivalIds.foreach {
          arrivalId =>
            val arrival = repo.get(arrivalId).futureValue.value
            arrival.messages.mapWithIndex {
              case (message, index) =>
                message.messageId.value mustBe (index + 1)
            }
        }
      }
    }
  }
}
