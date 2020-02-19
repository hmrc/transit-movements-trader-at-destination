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

package models
import java.time.LocalDate
import java.time.LocalTime

import models.messages.ArrivalNotificationMessage
import models.messages.MovementReferenceNumber
import play.api.libs.json.Json
import play.api.libs.json.OFormat

case class ArrivalMovement(internalReferenceId: Int, movementReferenceNumber: MovementReferenceNumber, messages: Seq[Message])

object ArrivalMovement {
  implicit val formats: OFormat[ArrivalMovement] = Json.format[ArrivalMovement]
}

sealed case class Message(
  date: LocalDate,
  time: LocalTime,
  message: ArrivalNotificationMessage
)
//TODO: The message type needs changing
//TODO: The ArrivalNotificationMessage isn't the full data that has been submitted to NCTS,
//TODO: this is just part of the message. Is their any requirement to save the full submission (the xml payload?)

object Message {
  implicit val formats: OFormat[Message] = Json.format[Message]
}
