/*
 * Copyright 2020 HM Revenue & Customs
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

import java.time.LocalDateTime

import controllers.routes
import models.MessageStatus.SubmissionFailed
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.MessageId
import models.MovementReferenceNumber
import play.api.libs.json.Json
import play.api.libs.json.OWrites

case class ResponseArrival(arrivalId: ArrivalId,
                           location: String,
                           messagesLocation: String,
                           movementReferenceNumber: MovementReferenceNumber,
                           status: ArrivalStatus,
                           created: LocalDateTime,
                           updated: LocalDateTime,
                           messages: Seq[ResponseMovementMessage])

object ResponseArrival {

  def build(arrival: Arrival): ResponseArrival = {
    val validMessages =
      arrival.messagesWithId
        .filterNot(_._1.optStatus.contains(SubmissionFailed))
        .map(messageData => ResponseMovementMessage.build(arrival.arrivalId, messageData._2, messageData._1))

    ResponseArrival(
      arrival.arrivalId,
      routes.MovementsController.getArrival(arrival.arrivalId).url,
      routes.MessagesController.getMessages(arrival.arrivalId).url,
      arrival.movementReferenceNumber,
      arrival.status,
      arrival.created,
      arrival.updated,
      validMessages
    )
  }

  implicit val writes: OWrites[ResponseArrival] = Json.writes[ResponseArrival]

}
