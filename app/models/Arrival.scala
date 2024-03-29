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

package models

import cats.data._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import models.MessageType._
import utils.MessageTypeUtils

case class Arrival(
  arrivalId: ArrivalId,
  channel: ChannelType,
  movementReferenceNumber: MovementReferenceNumber,
  eoriNumber: String,
  created: LocalDateTime,
  updated: LocalDateTime,
  lastUpdated: LocalDateTime,
  messages: NonEmptyList[MovementMessage],
  nextMessageCorrelationId: Int,
  notificationBox: Option[Box]
) {

  lazy val nextMessageId: MessageId = MessageId(messages.length + 1)

  lazy val messagesWithId: NonEmptyList[(MovementMessage, MessageId)] =
    messages.map(
      msg => msg -> msg.messageId
    )

  private val obfuscatedEori: String          = s"ending ${eoriNumber.takeRight(7)}"
  private val isoFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  val summaryInformation: Map[String, String] = Map(
    "Arrival id"                  -> arrivalId.index.toString,
    "Channel"                     -> channel.toString,
    "EORI"                        -> obfuscatedEori,
    "MRN"                         -> movementReferenceNumber.value,
    "Created"                     -> isoFormatter.format(created),
    "Last updated"                -> isoFormatter.format(lastUpdated),
    "Messages"                    -> messages.toList.map(_.messageType.toString).mkString(", "),
    "Next message correlation id" -> nextMessageCorrelationId.toString
  )

  val status: ArrivalStatus = MessageTypeUtils.status(messages.toList)
}

object Arrival {

  implicit def formatsNonEmptyList[A](implicit listReads: Reads[List[A]], listWrites: Writes[List[A]]): Format[NonEmptyList[A]] =
    new Format[NonEmptyList[A]] {
      override def writes(o: NonEmptyList[A]): JsValue = Json.toJson(o.toList)

      override def reads(json: JsValue): JsResult[NonEmptyList[A]] = json.validate(listReads).map(NonEmptyList.fromListUnsafe)
    }

  implicit val readsArrival: Reads[Arrival] =
    (
      (__ \ "_id").read[ArrivalId] and
        (__ \ "channel").read[ChannelType] and
        (__ \ "movementReferenceNumber").read[MovementReferenceNumber] and
        (__ \ "eoriNumber").read[String] and
        (__ \ "created").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "updated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "lastUpdated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "messages").read[NonEmptyList[MovementMessage]] and
        (__ \ "nextMessageCorrelationId").read[Int] and
        (__ \ "notificationBox").readNullable[Box]
    )(Arrival.apply _)

  implicit def writesArrival(implicit write: Writes[LocalDateTime]): OWrites[Arrival] =
    (
      (__ \ "_id").write[ArrivalId] and
        (__ \ "channel").write[ChannelType] and
        (__ \ "movementReferenceNumber").write[MovementReferenceNumber] and
        (__ \ "eoriNumber").write[String] and
        (__ \ "created").write(write) and
        (__ \ "updated").write(write) and
        (__ \ "lastUpdated").write(write) and
        (__ \ "messages").write[NonEmptyList[MovementMessage]] and
        (__ \ "nextMessageCorrelationId").write[Int] and
        (__ \ "notificationBox").writeNullable[Box]
    )(unlift(Arrival.unapply))

  implicit def formatsArrival(implicit write: Writes[LocalDateTime]): OFormat[Arrival] =
    OFormat(readsArrival, writesArrival)
}
