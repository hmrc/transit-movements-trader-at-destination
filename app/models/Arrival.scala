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

import models.request.ArrivalId
import play.api.libs.json._
import play.api.libs.functional.syntax._

import models.MongoDateTimeFormats._

case class Arrival(
  arrivalId: ArrivalId,
  movementReferenceNumber: MovementReferenceNumber,
  eoriNumber: String,
  state: State,
  created: LocalDateTime,
  updated: LocalDateTime,
  messages: Seq[MovementMessage]
)

object Arrival {

  implicit val readsArrival: Reads[Arrival] =
    (
      (__ \ "_id").read[ArrivalId] and
        (__ \ "movementReferenceNumber").read[MovementReferenceNumber] and
        (__ \ "eoriNumber").read[String] and
        (__ \ "state").read[State] and
        (__ \ "created").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "updated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "messages").read[Seq[MovementMessage]]
    )(Arrival.apply _)

  implicit def writesArrival(implicit write: Writes[LocalDateTime]): OWrites[Arrival] =
    (
      (__ \ "_id").write[ArrivalId] and
        (__ \ "movementReferenceNumber").write[MovementReferenceNumber] and
        (__ \ "eoriNumber").write[String] and
        (__ \ "state").write[State] and
        (__ \ "created").write(write) and
        (__ \ "updated").write(write) and
        (__ \ "messages").write[Seq[MovementMessage]]
    )(unlift(Arrival.unapply))

}
