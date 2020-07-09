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

import java.time.LocalDateTime

import cats._
import play.api.libs.json.Json
import play.api.libs.json.Writes

sealed trait ArrivalUpdate

object ArrivalUpdate {

  implicit val arrivalUpdateSemigroup: Semigroup[ArrivalUpdate] = {
    case (_: ArrivalStatusUpdate, x: ArrivalStatusUpdate)  => x
    case (a: ArrivalStatusUpdate, m: MessageStatusUpdate)  => CompoundStatusUpdate(a, m)
    case (_: ArrivalStatusUpdate, c: CompoundStatusUpdate) => c
    case (_: ArrivalStatusUpdate, p: ArrivalPutUpdate)     => p

    case (_: MessageStatusUpdate, x: MessageStatusUpdate)  => x
    case (m: MessageStatusUpdate, a: ArrivalStatusUpdate)  => CompoundStatusUpdate(a, m)
    case (_: MessageStatusUpdate, c: CompoundStatusUpdate) => c
    case (_: MessageStatusUpdate, p: ArrivalPutUpdate)     => p

    case (_: CompoundStatusUpdate, x: CompoundStatusUpdate) => x
    case (c: CompoundStatusUpdate, a: ArrivalStatusUpdate)  => c.copy(arrivalStatusUpdate = a)
    case (c: CompoundStatusUpdate, m: MessageStatusUpdate)  => c.copy(messageStatusUpdate = m)
    case (_: CompoundStatusUpdate, p: ArrivalPutUpdate)     => p

    case (_: ArrivalPutUpdate, x: ArrivalPutUpdate)     => x
    case (p: ArrivalPutUpdate, a: ArrivalStatusUpdate)  => p.copy(arrivalUpdate = p.arrivalUpdate.copy(arrivalStatusUpdate = a))
    case (p: ArrivalPutUpdate, m: MessageStatusUpdate)  => p.copy(arrivalUpdate = p.arrivalUpdate.copy(messageStatusUpdate = m))
    case (p: ArrivalPutUpdate, c: CompoundStatusUpdate) => p.copy(arrivalUpdate = c)
  }

  implicit val arrivalUpdateArrivalModifier: ArrivalModifier[ArrivalUpdate] = {
    case x: MessageStatusUpdate  => ArrivalModifier.toJson(x)
    case x: ArrivalStatusUpdate  => ArrivalModifier.toJson(x)
    case x: CompoundStatusUpdate => ArrivalModifier.toJson(x)
    case x: ArrivalPutUpdate     => ArrivalModifier.toJson(x)
  }
}

final case class MessageStatusUpdate(messageId: MessageId, messageStatus: MessageStatus) extends ArrivalUpdate

object MessageStatusUpdate extends MongoDateTimeFormats {
  implicit def arrivalStateUpdate(implicit writes: Writes[MessageStatus]): ArrivalModifier[MessageStatusUpdate] =
    ArrivalModifier(
      value =>
        Json.obj(
          "$set" ->
            Json.obj(
              s"messages.${value.messageId.index}.status" -> value.messageStatus,
              "lastUpdated"                               -> LocalDateTime.now.withSecond(0).withNano(0)
            )
      )
    )
}

final case class ArrivalStatusUpdate(arrivalStatus: ArrivalStatus) extends ArrivalUpdate

object ArrivalStatusUpdate extends MongoDateTimeFormats {
  implicit def arrivalStatusUpdate(implicit writes: Writes[ArrivalStatus]): ArrivalModifier[ArrivalStatusUpdate] =
    value =>
      Json.obj(
        "$set" -> Json.obj(
          "status"      -> value.arrivalStatus,
          "lastUpdated" -> LocalDateTime.now.withSecond(0).withNano(0)
        ))
}

final case class CompoundStatusUpdate(arrivalStatusUpdate: ArrivalStatusUpdate, messageStatusUpdate: MessageStatusUpdate) extends ArrivalUpdate

object CompoundStatusUpdate {
  implicit val arrivalUpdate: ArrivalModifier[CompoundStatusUpdate] =
    csu => ArrivalModifier.toJson(csu.arrivalStatusUpdate) deepMerge ArrivalModifier.toJson(csu.messageStatusUpdate)
}

final case class ArrivalPutUpdate(movementReferenceNumber: MovementReferenceNumber, arrivalUpdate: CompoundStatusUpdate) extends ArrivalUpdate

object ArrivalPutUpdate extends MongoDateTimeFormats {

  implicit val arrivalPutUpdateArrivalModifier: ArrivalModifier[ArrivalPutUpdate] = a =>
    Json.obj(
      "$set" -> Json.obj(
        "movementReferenceNumber" -> a.movementReferenceNumber,
        "lastUpdated"             -> LocalDateTime.now.withSecond(0).withNano(0)
      )
    ) deepMerge ArrivalModifier.toJson(a.arrivalUpdate)

}
