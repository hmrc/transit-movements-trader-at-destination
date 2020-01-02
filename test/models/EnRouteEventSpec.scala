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

import org.scalacheck.Arbitrary.arbitrary
import generators.ModelGenerators
import models.behaviours.JsonBehaviours
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class EnRouteEventSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "must serialise" in {

    forAll(arbitrary[EnRouteEvent]) {
      enrouteEvent =>
        val json = buildEnrouteEvent(enrouteEvent)

        Json.toJson(enrouteEvent) mustEqual json
    }
  }

  "must deserialise" in {

    forAll(arbitrary[EnRouteEvent]) {
      enrouteEvent =>
        val json = buildEnrouteEvent(enrouteEvent)

        json.validate[EnRouteEvent] mustEqual JsSuccess(enrouteEvent)
    }
  }

  def buildEnrouteEvent(enrouteEvent: EnRouteEvent): JsObject =
    Json.obj(
      "place"         -> enrouteEvent.place,
      "countryCode"   -> enrouteEvent.countryCode,
      "alreadyInNcts" -> enrouteEvent.alreadyInNcts,
      "eventDetails"  -> Json.toJson(enrouteEvent.eventDetails)
    ) ++ {
      enrouteEvent.seals match {
        case Some(seals) =>
          Json.obj("seals" -> Json.toJson(seals))
        case _ =>
          JsObject.empty
      }
    }
}
