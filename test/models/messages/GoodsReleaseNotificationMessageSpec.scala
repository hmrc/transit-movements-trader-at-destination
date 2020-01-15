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

package models.messages

import java.time.LocalDate

import generators.MessageGenerators
import models.behaviours.JsonBehaviours
import models.messages
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class GoodsReleaseNotificationMessageSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with MessageGenerators with JsonBehaviours {

  mustHaveDualReadsAndWrites(arbitrary[GoodsReleaseNotificationMessage])

  "must deserialise" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(arbitrary[String], date, arbitrary[Trader], arbitrary[String]) {
      (mrn, releaseDate, trader, presentationOffice) =>
        val json = Json.obj(
          "movementReferenceNumber" -> mrn,
          "releaseDate"             -> releaseDate,
          "trader"                  -> trader,
          "presentationOffice"      -> presentationOffice
        )

        val expectedResult = messages.GoodsReleaseNotificationMessage(mrn, releaseDate, trader, presentationOffice)

        json.validate[GoodsReleaseNotificationMessage] mustEqual JsSuccess(expectedResult)
    }
  }

  "must serialise" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(arbitrary[String], date, arbitrary[Trader], arbitrary[String]) {
      (mrn, releaseDate, trader, presentationOffice) =>
        val json = Json.obj(
          "movementReferenceNumber" -> mrn,
          "releaseDate"             -> releaseDate,
          "trader"                  -> trader,
          "presentationOffice"      -> presentationOffice
        )

        val notification = messages.GoodsReleaseNotificationMessage(mrn, releaseDate, trader, presentationOffice)

        Json.toJson(notification) mustEqual json
    }
  }
}
