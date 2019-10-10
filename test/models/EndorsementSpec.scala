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

import java.time.LocalDate

import generators.ModelGenerators
import models.behaviours.JsonBehaviours
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsSuccess, Json}

class EndorsementSpec extends FreeSpec with MustMatchers
  with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  mustHaveDualReadsAndWrites(arbitrary[Endorsement])

  "must deserialise when nothing is present" in {

    Json.obj().validate[Endorsement] mustEqual JsSuccess(Endorsement(None, None, None, None))
  }

  "must deserialise when all values are present" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(date, arbitrary[String], arbitrary[String], arbitrary[String]) {
      (date, authority, place, country) =>

        val json = Json.obj(
          "date"      -> date,
          "authority" -> authority,
          "place"     -> place,
          "country"   -> country
        )

        val expectedResult = Endorsement(Some(date), Some(authority), Some(place), Some(country))

        json.validate[Endorsement] mustEqual JsSuccess(expectedResult)
    }
  }

  "must serialise when nothing is present" in {

    Json.toJson(Endorsement(None, None, None, None)) mustEqual Json.obj()
  }

  "must serialise when all values are present" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(date, arbitrary[String], arbitrary[String], arbitrary[String]) {
      (date, authority, place, country) =>

        val json = Json.obj(
          "date"      -> date,
          "authority" -> authority,
          "place"     -> place,
          "country"   -> country
        )

        val endorsement = Endorsement(Some(date), Some(authority), Some(place), Some(country))

        Json.toJson(endorsement) mustEqual json
    }
  }
}

