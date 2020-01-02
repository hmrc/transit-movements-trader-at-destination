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

import play.api.libs.json._

final case class EnRouteEvent(
  place: String,
  countryCode: String,
  alreadyInNcts: Boolean,
  eventDetails: EventDetails,
  seals: Option[Seq[String]]
)

object EnRouteEvent {

  implicit lazy val reads: Reads[EnRouteEvent] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "place").read[String] and
        (__ \ "countryCode").read[String] and
        (__ \ "alreadyInNcts").read[Boolean] and
        (__ \ "eventDetails").read[EventDetails] and
        (__ \ "seals").readNullable[Seq[String]]
    )(EnRouteEvent(_, _, _, _, _))
  }

  implicit lazy val writes: OWrites[EnRouteEvent] =
    OWrites[EnRouteEvent] {
      event =>
        Json
          .obj(
            "place"         -> event.place,
            "countryCode"   -> event.countryCode,
            "alreadyInNcts" -> event.alreadyInNcts,
            "eventDetails"  -> Json.toJson(event.eventDetails),
            "seals"         -> Json.toJson(event.seals)
          )
          .filterNulls
    }
}
