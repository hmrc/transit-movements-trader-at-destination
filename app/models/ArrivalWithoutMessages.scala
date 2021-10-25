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

package models

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.__
import play.api.libs.functional.syntax._
import utils.MessageTypeUtils

case class ArrivalWithoutMessages(
  arrivalId: ArrivalId,
  channel: ChannelType,
  movementReferenceNumber: MovementReferenceNumber,
  eoriNumber: String,
  created: LocalDateTime,
  updated: LocalDateTime,
  lastUpdated: LocalDateTime = LocalDateTime.now,
  notificationBox: Option[Box],
  nextMessageId: MessageId,
  nextMessageCorrelationId: Int,
  messagesMetaData: Seq[MessageMetaData]
) {
  private val obfuscatedEori: String          = s"ending ${eoriNumber.takeRight(7)}"
  private val isoFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  val summaryInformation: Map[String, String] = Map(
    "Arrival id"                  -> arrivalId.index.toString,
    "Channel"                     -> channel.toString,
    "EORI"                        -> obfuscatedEori,
    "MRN"                         -> movementReferenceNumber.value,
    "Created"                     -> isoFormatter.format(created),
    "Last updated"                -> isoFormatter.format(lastUpdated),
    "Next message correlation id" -> nextMessageCorrelationId.toString
  )

  def currentStatus: ArrivalStatus = MessageTypeUtils.currentArrivalStatus(messagesMetaData.toList)
}

object ArrivalWithoutMessages {

  def fromArrival(arrival: Arrival): ArrivalWithoutMessages =
    ArrivalWithoutMessages(
      arrival.arrivalId,
      arrival.channel,
      arrival.movementReferenceNumber,
      arrival.eoriNumber,
      arrival.created,
      arrival.updated,
      arrival.lastUpdated,
      arrival.notificationBox,
      arrival.nextMessageId,
      arrival.nextMessageCorrelationId,
      arrival.messages.map(x => MessageMetaData(x.messageType, x.dateTime)).toList
    )

  implicit val readsArrival: Reads[ArrivalWithoutMessages] = {
    (
      (__ \ "_id").read[ArrivalId] and
        (__ \ "channel").read[ChannelType] and
        (__ \ "movementReferenceNumber").read[MovementReferenceNumber] and
        (__ \ "eoriNumber").read[String] and
        (__ \ "created").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "updated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "lastUpdated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "notificationBox").readNullable[Box] and
        (__ \ "nextMessageId").read[MessageId] and
        (__ \ "nextMessageCorrelationId").read[Int] and
        (__ \ "messages").read[Seq[MessageMetaData]]
    )(ArrivalWithoutMessages.apply _)
  }

  val projection: JsObject = Json.obj(
    "_id"                      -> 1,
    "channel"                  -> 1,
    "eoriNumber"               -> 1,
    "movementReferenceNumber"  -> 1,
    "created"                  -> 1,
    "updated"                  -> 1,
    "lastUpdated"              -> 1,
    "notificationBox"          -> 1,
    "nextMessageCorrelationId" -> 1,
    "messages"                 -> 1
  )
}
