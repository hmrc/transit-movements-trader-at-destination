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

import play.api.libs.json.Reads
import play.api.libs.json.__
import play.api.libs.functional.syntax._

case class ArrivalWithoutMessages(
  arrivalId: ArrivalId,
  channel: ChannelType,
  movementReferenceNumber: MovementReferenceNumber,
  eoriNumber: String,
  status: ArrivalStatus,
  created: LocalDateTime,
  updated: LocalDateTime,
  lastUpdated: LocalDateTime = LocalDateTime.now,
  notificationBox: Option[Box]
)

object ArrivalWithoutMessages {

  def fromArrival(arrival: Arrival): ArrivalWithoutMessages =
    ArrivalWithoutMessages(
      arrival.arrivalId,
      arrival.channel,
      arrival.movementReferenceNumber,
      arrival.eoriNumber,
      arrival.status,
      arrival.created,
      arrival.updated,
      arrival.lastUpdated,
      arrival.notificationBox
    )

  implicit val readsArrival: Reads[ArrivalWithoutMessages] =
    (
      (__ \ "_id").read[ArrivalId] and
        (__ \ "channel").read[ChannelType] and
        (__ \ "movementReferenceNumber").read[MovementReferenceNumber] and
        (__ \ "eoriNumber").read[String] and
        (__ \ "status").read[ArrivalStatus] and
        (__ \ "created").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "updated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "lastUpdated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "notificationBox").readNullable[Box]
    )(ArrivalWithoutMessages.apply _)
}
