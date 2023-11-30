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

import models.MovementMessage.format
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

case class ArrivalMessages(
  arrivalId: ArrivalId,
  eoriNumber: String,
  messages: List[MovementMessage]
)

object ArrivalMessages {

  implicit val reads: Reads[ArrivalMessages] =
    (
      (__ \ "_id").read[ArrivalId] and
        (__ \ "eoriNumber").read[String] and
        (__ \ "messages").read[List[MovementMessage]]
    )(ArrivalMessages.apply _)

  implicit val writes: OWrites[ArrivalMessages] =
    (
      (__ \ "_id").write[ArrivalId] and
        (__ \ "eoriNumber").write[String] and
        (__ \ "messages").write[List[MovementMessage]]
    )(unlift(ArrivalMessages.unapply))

  implicit val formatsArrival: Format[ArrivalMessages] =
    Format(reads, writes)

  val projection: JsObject = Json.obj(
    "_id"        -> 1,
    "eoriNumber" -> 1,
    "messages"   -> 1
  )
}
