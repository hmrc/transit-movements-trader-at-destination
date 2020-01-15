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

package models.domain

import generators.ModelGenerators
import models.behaviours.JsonBehaviours
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsError
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class TraderSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "Trader with EORI" - {

    mustHaveDualReadsAndWrites(arbitrary[TraderWithEori])

    "must deserialise when address fields are present" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (eori, name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "eori"            -> eori,
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          val expectedResult = TraderWithEori(eori, Some(name), Some(streetAndNumber), Some(postCode), Some(city), Some(countryCode))

          json.validate[TraderWithEori] mustEqual JsSuccess(expectedResult)
      }
    }

    "must deserialise when only EORI is present" in {

      forAll(arbitrary[String]) {
        eori =>
          val json = Json.obj("eori" -> eori)

          val expectedResult = TraderWithEori(eori, None, None, None, None, None)

          json.validate[TraderWithEori] mustEqual JsSuccess(expectedResult)
      }
    }

    "must fail to deserialise when EORI is not present" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          json.validate[TraderWithEori] mustBe a[JsError]
      }
    }

    "must serialise when all address fields are present" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (eori, name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "eori"            -> eori,
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          val trader = TraderWithEori(eori, Some(name), Some(streetAndNumber), Some(postCode), Some(city), Some(countryCode))

          Json.toJson(trader) mustEqual json
      }
    }

    "must serialise when only EORI is present" in {

      forAll(arbitrary[String]) {
        eori =>
          val json = Json.obj("eori" -> eori)

          val trader = TraderWithEori(eori, None, None, None, None, None)

          Json.toJson(trader) mustEqual json
      }
    }
  }

  "Trader without EORI" - {

    mustHaveDualReadsAndWrites(arbitrary[TraderWithoutEori])

    "must deserialise" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          val expectedResult = TraderWithoutEori(name, streetAndNumber, postCode, city, countryCode)

          json.validate[TraderWithoutEori] mustEqual JsSuccess(expectedResult)
      }
    }

    "must serialise" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          val trader = TraderWithoutEori(name, streetAndNumber, postCode, city, countryCode)

          Json.toJson(trader) mustEqual json
      }
    }
  }

  "Trader" - {

    "must deserialise to a Trader with EORI" - {

      "when all address fields are present" in {

        forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
          (eori, name, streetAndNumber, postCode, city, countryCode) =>
            val json = Json.obj(
              "eori"            -> eori,
              "name"            -> name,
              "streetAndNumber" -> streetAndNumber,
              "postCode"        -> postCode,
              "city"            -> city,
              "countryCode"     -> countryCode
            )

            val expectedResult = TraderWithEori(eori, Some(name), Some(streetAndNumber), Some(postCode), Some(city), Some(countryCode))

            json.validate[Trader] mustEqual JsSuccess(expectedResult)
        }
      }

      "when only the EORI is present" in {

        forAll(arbitrary[String]) {
          eori =>
            val json = Json.obj("eori" -> eori)

            val expectedResult = TraderWithEori(eori, None, None, None, None, None)

            json.validate[Trader] mustEqual JsSuccess(expectedResult)
        }
      }
    }

    "must deserialise to a Trader without EORI" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          val expectedResult = TraderWithoutEori(name, streetAndNumber, postCode, city, countryCode)

          json.validate[Trader] mustEqual JsSuccess(expectedResult)
      }
    }

    "must serialise to a Trader with EORI" - {

      "when all address fields are present" in {

        forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
          (eori, name, streetAndNumber, postCode, city, countryCode) =>
            val json = Json.obj(
              "eori"            -> eori,
              "name"            -> name,
              "streetAndNumber" -> streetAndNumber,
              "postCode"        -> postCode,
              "city"            -> city,
              "countryCode"     -> countryCode
            )

            val trader = TraderWithEori(eori, Some(name), Some(streetAndNumber), Some(postCode), Some(city), Some(countryCode))

            Json.toJson(trader: Trader) mustEqual json
        }
      }

      "when only the EORI is present" in {

        forAll(arbitrary[String]) {
          eori =>
            val json = Json.obj("eori" -> eori)

            val trader = TraderWithEori(eori, None, None, None, None, None)

            Json.toJson(trader: Trader) mustEqual json
        }
      }
    }

    "must serialise to a Trader without EORI" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String], arbitrary[String]) {
        (name, streetAndNumber, postCode, city, countryCode) =>
          val json = Json.obj(
            "name"            -> name,
            "streetAndNumber" -> streetAndNumber,
            "postCode"        -> postCode,
            "city"            -> city,
            "countryCode"     -> countryCode
          )

          val trader = TraderWithoutEori(name, streetAndNumber, postCode, city, countryCode)

          Json.toJson(trader: Trader) mustEqual json
      }
    }
  }
}
