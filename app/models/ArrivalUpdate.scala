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
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes

case class MessageStatusUpdate(messageId: MessageId, messageStatus: MessageStatus)

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

case class ArrivalUpdate(arrivalUpdate: Option[ArrivalStatus], messageUpdate: Option[MessageStatusUpdate])

object ArrivalUpdate {

  implicit object ArrivalUpdateSemiGroupInst extends Semigroup[ArrivalUpdate] {
    override def combine(x: ArrivalUpdate, y: ArrivalUpdate): ArrivalUpdate =
      ArrivalUpdate(
        y.arrivalUpdate orElse x.arrivalUpdate,
        y.messageUpdate orElse x.messageUpdate
      )
  }

  implicit object ArrivalUpdateArrivalModifier extends ArrivalModifier[ArrivalUpdate] {
    override def toJson(a: ArrivalUpdate): JsObject = {

      val arrivalUpdateJson: JsObject = a.arrivalUpdate.map(ArrivalModifier.toJson[ArrivalStatus]).getOrElse(Json.obj())
      val messageUpdateJson: JsObject = a.messageUpdate.map(ArrivalModifier.toJson[MessageStatusUpdate]).getOrElse(Json.obj())

      arrivalUpdateJson deepMerge messageUpdateJson
    }
  }

}
