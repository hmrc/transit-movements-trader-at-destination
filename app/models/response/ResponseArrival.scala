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

import controllers.routes
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.ArrivalWithoutMessages
import models.MessageType
import models.MongoDateTimeFormats
import models.MovementReferenceNumber
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import play.api.libs.json.Reads

import java.time.LocalDateTime

case class ResponseArrival(arrivalId: ArrivalId,
                           location: String,
                           messagesLocation: String,
                           movementReferenceNumber: MovementReferenceNumber,
                           status: ArrivalStatus,
                           created: LocalDateTime,
                           updated: LocalDateTime,
                           latestMessage: MessageType)

object ResponseArrival {

  def build(arrival: Arrival): ResponseArrival =
    ResponseArrival(
      arrival.arrivalId,
      routes.MovementsController.getArrival(arrival.arrivalId).url,
      routes.MessagesController.getMessages(arrival.arrivalId).url,
      arrival.movementReferenceNumber,
      arrival.status,
      arrival.created,
      updated = arrival.lastUpdated,
      arrival.latestMessage
    )

  def build(arrivalWithoutMessages: ArrivalWithoutMessages): ResponseArrival =
    ResponseArrival(
      arrivalWithoutMessages.arrivalId,
      routes.MovementsController.getArrival(arrivalWithoutMessages.arrivalId).url,
      routes.MessagesController.getMessages(arrivalWithoutMessages.arrivalId).url,
      arrivalWithoutMessages.movementReferenceNumber,
      arrivalWithoutMessages.status,
      arrivalWithoutMessages.created,
      updated = arrivalWithoutMessages.lastUpdated,
      arrivalWithoutMessages.latestMessage
    )

  val projection: JsObject = Json.obj(
    "_id"                     -> 1,
    "movementReferenceNumber" -> 1,
    "status"                  -> 1,
    "created"                 -> 1,
    "lastUpdated"             -> 1
  )

  implicit def reads: Reads[ResponseArrival] =
    json =>
      for {
        arrivalId <- (json \ "_id").validate[ArrivalId]
        location         = routes.MovementsController.getArrival(arrivalId).url
        messagesLocation = routes.MessagesController.getMessages(arrivalId).url
        movementReferenceNumber <- (json \ "movementReferenceNumber").validate[MovementReferenceNumber]
        status                  <- (json \ "status").validate[ArrivalStatus]
        created                 <- (json \ "created").validate[LocalDateTime](MongoDateTimeFormats.localDateTimeRead)
        updated                 <- (json \ "lastUpdated").validate[LocalDateTime](MongoDateTimeFormats.localDateTimeRead)
        latestMessage           <- (json \ "latestMessage").validate[MessageType]
      } yield
        ResponseArrival(
          arrivalId,
          location,
          messagesLocation,
          movementReferenceNumber,
          status,
          created,
          updated,
          latestMessage
      )

  implicit val writes: OWrites[ResponseArrival] = Json.writes[ResponseArrival]

}
