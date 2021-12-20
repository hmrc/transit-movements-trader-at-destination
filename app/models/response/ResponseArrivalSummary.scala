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

package models.response

import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.ArrivalWithoutMessages
import models.MessageMetaData
import models.MongoDateTimeFormats
import models.MovementReferenceNumber
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import play.api.libs.json.Reads
import java.time.LocalDateTime

import utils.MessageTypeUtils

case class ResponseArrivalSummary(
  arrivalId: ArrivalId,
  movementReferenceNumber: MovementReferenceNumber,
  currentStatus: ArrivalStatus,
  previousStatus: ArrivalStatus,
  created: LocalDateTime,
  updated: LocalDateTime
)

object ResponseArrivalSummary {

  def build(arrival: Arrival): ResponseArrivalSummary =
    ResponseArrivalSummary(
      arrivalId = arrival.arrivalId,
      movementReferenceNumber = arrival.movementReferenceNumber,
      currentStatus = arrival.currentStatus,
      previousStatus = arrival.previousStatus,
      created = arrival.created,
      updated = arrival.lastUpdated,
    )

  def build(arrivalWithoutMessages: ArrivalWithoutMessages): ResponseArrivalSummary =
    ResponseArrivalSummary(
      arrivalId = arrivalWithoutMessages.arrivalId,
      movementReferenceNumber = arrivalWithoutMessages.movementReferenceNumber,
      currentStatus = arrivalWithoutMessages.currentStatus,
      previousStatus = arrivalWithoutMessages.previousStatus,
      created = arrivalWithoutMessages.created,
      updated = arrivalWithoutMessages.lastUpdated
    )

  val projection: JsObject = Json.obj(
    "_id"                     -> 1,
    "movementReferenceNumber" -> 1,
    "created"                 -> 1,
    "lastUpdated"             -> 1
  )

  implicit def reads: Reads[ResponseArrivalSummary] =
    json =>
      for {
        arrivalId               <- (json \ "_id").validate[ArrivalId]
        movementReferenceNumber <- (json \ "movementReferenceNumber").validate[MovementReferenceNumber]
        created                 <- (json \ "created").validate[LocalDateTime](MongoDateTimeFormats.localDateTimeRead)
        updated                 <- (json \ "lastUpdated").validate[LocalDateTime](MongoDateTimeFormats.localDateTimeRead)
        latestMessage           <- (json \ "messagesMetaData").validate[Seq[MessageMetaData]]
      } yield {
        val currentStatus: ArrivalStatus  = MessageTypeUtils.currentArrivalStatus(latestMessage.toList)
        val previousStatus: ArrivalStatus = MessageTypeUtils.previousArrivalStatus(latestMessage.toList, currentStatus)
        ResponseArrivalSummary(
          arrivalId,
          movementReferenceNumber,
          currentStatus,
          previousStatus,
          created,
          updated
        )
    }

  implicit val writes: OWrites[ResponseArrivalSummary] = Json.writes[ResponseArrivalSummary]

}
