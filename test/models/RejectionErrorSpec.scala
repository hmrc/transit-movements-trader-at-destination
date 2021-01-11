/*
 * Copyright 2021 HM Revenue & Customs
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

import generators.ModelGenerators
import models.behaviours.JsonBehaviours
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class RejectionErrorSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  mustHaveDualReadsAndWrites(arbitrary[RejectionError])

  "must deserialise with all values present" in {

    forAll(arbitrary[Int], arbitrary[String], arbitrary[String], arbitrary[String]) {
      (errorType, pointer, reason, original) =>
        val json = Json.obj(
          "errorType"     -> errorType,
          "pointer"       -> pointer,
          "reason"        -> reason,
          "originalValue" -> original
        )

        val expectedResult = RejectionError(errorType, pointer, Some(reason), Some(original))

        json.validate[RejectionError] mustEqual JsSuccess(expectedResult)
    }
  }

  "must deserialise when optional values are not present" in {

    forAll(arbitrary[Int], arbitrary[String]) {
      (errorType, pointer) =>
        val json = Json.obj(
          "errorType" -> errorType,
          "pointer"   -> pointer
        )

        val expectedResult = RejectionError(errorType, pointer, None, None)

        json.validate[RejectionError] mustEqual JsSuccess(expectedResult)
    }
  }

  "must serialise with all values present" in {

    forAll(arbitrary[Int], arbitrary[String], arbitrary[String], arbitrary[String]) {
      (errorType, pointer, reason, original) =>
        val json = Json.obj(
          "errorType"     -> errorType,
          "pointer"       -> pointer,
          "reason"        -> reason,
          "originalValue" -> original
        )

        val error = RejectionError(errorType, pointer, Some(reason), Some(original))

        Json.toJson(error) mustEqual json
    }
  }

  "must serialise when optional values are not present" in {

    forAll(arbitrary[Int], arbitrary[String]) {
      (errorType, pointer) =>
        val json = Json.obj(
          "errorType" -> errorType,
          "pointer"   -> pointer
        )

        val error = RejectionError(errorType, pointer, None, None)

        Json.toJson(error) mustEqual json
    }
  }
}
