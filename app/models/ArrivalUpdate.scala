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

import java.time.Clock
import java.time.LocalDateTime

import cats._
import play.api.libs.json.Json
import play.api.libs.json.Writes

sealed trait ArrivalUpdate

object ArrivalUpdate {

  implicit val arrivalUpdateSemigroup: Semigroup[ArrivalUpdate] = {

    case (_: MessageStatusUpdate, x: MessageStatusUpdate) => x
    case (_: MessageStatusUpdate, p: ArrivalPutUpdate)    => p

    case (_: ArrivalPutUpdate, x: ArrivalPutUpdate)    => x
    case (p: ArrivalPutUpdate, m: MessageStatusUpdate) => p.copy(messageStatusUpdate = m)
  }

  implicit def arrivalUpdateArrivalModifier(implicit clock: Clock): ArrivalModifier[ArrivalUpdate] = {
    case x: MessageStatusUpdate => ArrivalModifier.toJson(x)
    case x: ArrivalPutUpdate    => ArrivalModifier.toJson(x)
  }
}

final case class MessageStatusUpdate(messageId: MessageId, messageStatus: MessageStatus) extends ArrivalUpdate

object MessageStatusUpdate extends MongoDateTimeFormats {

  implicit def arrivalStateUpdate(implicit clock: Clock, writes: Writes[MessageStatus]): ArrivalModifier[MessageStatusUpdate] =
    ArrivalModifier(
      value =>
        Json.obj(
          "$set" ->
            Json.obj(
              s"messages.${value.messageId.index}.status" -> value.messageStatus,
              "lastUpdated"                               -> LocalDateTime.now(clock)
            )
      )
    )
}

final case class ArrivalPutUpdate(movementReferenceNumber: MovementReferenceNumber, messageStatusUpdate: MessageStatusUpdate) extends ArrivalUpdate

object ArrivalPutUpdate extends MongoDateTimeFormats {

  implicit def arrivalPutUpdateArrivalModifier(implicit clock: Clock): ArrivalModifier[ArrivalPutUpdate] =
    a =>
      Json.obj(
        "$set" -> Json.obj(
          "movementReferenceNumber" -> a.movementReferenceNumber,
          "lastUpdated"             -> LocalDateTime.now(clock)
        )
      ) deepMerge ArrivalModifier.toJson(a.messageStatusUpdate)

}
