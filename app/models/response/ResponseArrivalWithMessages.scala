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
import models.MovementReferenceNumber
import models.MessageStatus.SubmissionFailed
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import java.time.LocalDateTime
import java.time.OffsetDateTime

case class ResponseArrivalWithMessages(
  arrivalId: ArrivalId,
  location: String,
  messagesLocation: String,
  movementReferenceNumber: MovementReferenceNumber,
  status: ArrivalStatus,
  created: LocalDateTime,
  updated: LocalDateTime,
  messages: Seq[ResponseMovementMessage]
)

object ResponseArrivalWithMessages {

  def build(arrival: Arrival, receivedSince: Option[OffsetDateTime]): ResponseArrivalWithMessages =
    ResponseArrivalWithMessages(
      arrival.arrivalId,
      routes.MovementsController.getArrival(arrival.arrivalId).url,
      routes.MessagesController.getMessages(arrival.arrivalId).url,
      arrival.movementReferenceNumber,
      arrival.currentStatus,
      arrival.created,
      updated = arrival.lastUpdated,
      arrival.messagesWithId
        .filterNot {
          case (message, _) =>
            val failedMessage            = message.optStatus == Some(SubmissionFailed)
            lazy val beforeRequestedDate = receivedSince.fold(false)(message.receivedBefore)
            failedMessage || beforeRequestedDate
        }
        .map {
          case (message, messageId) => ResponseMovementMessage.build(arrival.arrivalId, messageId, message)
        }
    )

  implicit val writes: OWrites[ResponseArrivalWithMessages] = Json.writes[ResponseArrivalWithMessages]
}
