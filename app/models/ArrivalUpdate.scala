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

import cats._
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes

sealed trait ArrivalUpdate

case class MessageStatusUpdate(messageId: MessageId, messageStatus: MessageStatus) extends ArrivalUpdate

object MessageStatusUpdate {
  implicit def arrivalStateUpdate(implicit writes: Writes[MessageStatus]): ArrivalModifier[MessageStatusUpdate] =
    ArrivalModifier(
      value =>
        Json.obj(
          "$set" ->
            Json.obj(
              s"messages.${value.messageId.index}.status" -> value.messageStatus
            )
      )
    )
}

case class ArrivalStatusUpdate(arrivalStatus: ArrivalStatus) extends ArrivalUpdate

object ArrivalStatusUpdate {
  implicit def arrivalStatusUpdate(implicit writes: Writes[ArrivalStatus]): ArrivalModifier[ArrivalStatusUpdate] =
    value =>
      Json.obj(
        "$set" -> Json.obj(
          "status" -> value.arrivalStatus
        ))
}

case class CompoundStatusUpdate(arrivalStatusUpdate: ArrivalStatusUpdate, messageStatusUpdate: MessageStatusUpdate) extends ArrivalUpdate

object CompoundStatusUpdate {
  implicit val arrivalUpdate: ArrivalModifier[CompoundStatusUpdate] =
    csu => ArrivalModifier.toJson(csu.arrivalStatusUpdate) deepMerge ArrivalModifier.toJson(csu.messageStatusUpdate)
}

case class ArrivalPutUpdate(movementReferenceNumber: MovementReferenceNumber, arrivalUpdate: CompoundStatusUpdate) extends ArrivalUpdate

object ArrivalPutUpdate {

  def selector(arrivalId: ArrivalId): JsObject = Json.obj(
    "_id" -> arrivalId
  )

  implicit val arrivalPutUpdateArrivalModifier: ArrivalModifier[ArrivalPutUpdate] = a =>
    Json.obj(
      "$set" -> Json.obj(
        "movementReferenceNumber" -> a.movementReferenceNumber
      )
    ) deepMerge ArrivalModifier.toJson(a.arrivalUpdate)

}
