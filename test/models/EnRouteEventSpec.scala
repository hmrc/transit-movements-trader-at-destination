/*
 * Copyright 2019 HM Revenue & Customs
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
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsSuccess, Json}

class EnRouteEventSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators {

  "must deserialise when no seals are present" in {

    forAll(arbitrary[String], arbitrary[String], arbitrary[Boolean], arbitrary[EventDetails]) {
      (place, countryCode, alreadyInNcts, eventDetails) =>

        val json = Json.obj(
          "place"         -> place,
          "countryCode"   -> countryCode,
          "alreadyInNcts" -> alreadyInNcts,
          "eventDetails"  -> Json.toJson(eventDetails)
        )

        val expectedResult = EnRouteEvent(place, countryCode, alreadyInNcts, eventDetails, Seq.empty)

        json.validate[EnRouteEvent] mustEqual JsSuccess(expectedResult)
    }
  }

  "must deserialise when seals are present" in {

    forAll(arbitrary[String], arbitrary[String], arbitrary[Boolean], arbitrary[EventDetails], arbitrary[Seq[String]]) {
      (place, countryCode, alreadyInNcts, eventDetails, seals) =>

        val json = Json.obj(
          "place"         -> place,
          "countryCode"   -> countryCode,
          "alreadyInNcts" -> alreadyInNcts,
          "eventDetails"  -> Json.toJson(eventDetails),
          "seals"         -> Json.toJson(seals)
        )

        val expectedResult = EnRouteEvent(place, countryCode, alreadyInNcts, eventDetails, seals)

        json.validate[EnRouteEvent] mustEqual JsSuccess(expectedResult)
    }
  }

  "must serialise" in {

    forAll(arbitrary[String], arbitrary[String], arbitrary[Boolean], arbitrary[EventDetails], arbitrary[Seq[String]]) {
      (place, countryCode, alreadyInNcts, eventDetails, seals) =>

        val json = if (seals.isEmpty) {
          Json.obj(
            "place"         -> place,
            "countryCode"   -> countryCode,
            "alreadyInNcts" -> alreadyInNcts,
            "eventDetails"  -> Json.toJson(eventDetails)
          )
        } else {
          Json.obj(
            "place"         -> place,
            "countryCode"   -> countryCode,
            "alreadyInNcts" -> alreadyInNcts,
            "eventDetails"  -> Json.toJson(eventDetails),
            "seals"         -> Json.toJson(seals)
          )
        }

        val event = EnRouteEvent(place, countryCode, alreadyInNcts, eventDetails, seals)

        Json.toJson(event) mustEqual json
    }
  }
}
