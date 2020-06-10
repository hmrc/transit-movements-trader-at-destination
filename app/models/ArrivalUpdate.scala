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

object ArrivalUpdate {

  implicit val arrivalUpdateSemigroup: Semigroup[ArrivalUpdate] = {
    case (ArrivalStatusUpdate(_), x @ ArrivalStatusUpdate(_))        => x
    case (a @ ArrivalStatusUpdate(_), m @ MessageStatusUpdate(_, _)) => CompoundStatusUpdate(a, m)
    case (ArrivalStatusUpdate(_), c @ CompoundStatusUpdate(_, _))    => c
    case (ArrivalStatusUpdate(_), p @ ArrivalPutUpdate(_, _))        => p

    case (MessageStatusUpdate(_, _), x @ MessageStatusUpdate(_, _))  => x
    case (m @ MessageStatusUpdate(_, _), a @ ArrivalStatusUpdate(_)) => CompoundStatusUpdate(a, m)
    case (MessageStatusUpdate(_, _), c @ CompoundStatusUpdate(_, _)) => c
    case (MessageStatusUpdate(_, _), p @ ArrivalPutUpdate(_, _))     => p

    case (CompoundStatusUpdate(_, _), x @ CompoundStatusUpdate(_, _))    => x
    case (c @ CompoundStatusUpdate(_, _), a @ ArrivalStatusUpdate(_))    => c.copy(arrivalStatusUpdate = a)
    case (c @ CompoundStatusUpdate(_, _), m @ MessageStatusUpdate(_, _)) => c.copy(messageStatusUpdate = m)
    case (CompoundStatusUpdate(_, _), p @ ArrivalPutUpdate(_, _))        => p

    case (ArrivalPutUpdate(_, _), x @ ArrivalPutUpdate(_, _))         => x
    case (p @ ArrivalPutUpdate(_, _), a @ ArrivalStatusUpdate(_))     => p.copy(arrivalUpdate = p.arrivalUpdate.copy(arrivalStatusUpdate = a))
    case (p @ ArrivalPutUpdate(_, _), m @ MessageStatusUpdate(_, _))  => p.copy(arrivalUpdate = p.arrivalUpdate.copy(messageStatusUpdate = m))
    case (p @ ArrivalPutUpdate(_, _), c @ CompoundStatusUpdate(_, _)) => p.copy(arrivalUpdate = c)
  }

  implicit val arrivalUpdateArrivalModifier: ArrivalModifier[ArrivalUpdate] = {
    case x @ MessageStatusUpdate(_, _)  => ArrivalModifier.toJson(x)
    case x @ ArrivalStatusUpdate(_)     => ArrivalModifier.toJson(x)
    case x @ CompoundStatusUpdate(_, _) => ArrivalModifier.toJson(x)
    case x @ ArrivalPutUpdate(_, _)     => ArrivalModifier.toJson(x)
  }
}

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
