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

package models.messages

import java.time.LocalDate

import generators.ModelGenerators
import models._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsSuccess, Json}

class ArrivalNotificationRejectionSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators {

  "must deserialise when action, reason and errors are not present" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(arbitrary[String], date) {
      (mrn, rejectionDate) =>

        val json = Json.obj(
          "movementReferenceNumber" -> mrn,
          "rejectionDate"           -> rejectionDate
        )

        val expectedResult = ArrivalNotificationRejection(mrn, rejectionDate, None, None, Seq.empty)

        json.validate[ArrivalNotificationRejection] mustEqual JsSuccess(expectedResult)
    }
  }

  "must deserialise when action, reason and errors are present" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(arbitrary[String], date, arbitrary[Option[String]], arbitrary[Option[String]], arbitrary[Seq[RejectionError]]) {
      (mrn, rejectionDate, action, reason, errors) =>

        val json = Json.obj(
          "movementReferenceNumber" -> mrn,
          "rejectionDate"           -> rejectionDate,
          "action"                  -> action,
          "reason"                  -> reason,
          "errors"                  -> Json.toJson(errors)
        )

        val expectedResult = ArrivalNotificationRejection(mrn, rejectionDate, action, reason, errors)

        json.validate[ArrivalNotificationRejection] mustEqual JsSuccess(expectedResult)
    }
  }

  "must serialise when action, reason and errors are not present" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(arbitrary[String], date) {
      (mrn, rejectionDate) =>

        val json = Json.obj(
          "movementReferenceNumber" -> mrn,
          "rejectionDate"           -> rejectionDate
        )

        val rejection = ArrivalNotificationRejection(mrn, rejectionDate, None, None, Seq.empty)

        Json.toJson(rejection) mustEqual json
    }
  }

  "must serialise when action, reason and errors are present" in {

    val date = datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)

    forAll(arbitrary[String], date, arbitrary[String], arbitrary[String], arbitrary[Seq[RejectionError]]) {
      (mrn, rejectionDate, action, reason, errors) =>

        val json = if (errors.isEmpty) {
          Json.obj(
            "movementReferenceNumber" -> mrn,
            "rejectionDate" -> rejectionDate,
            "action" -> action,
            "reason" -> reason
          )
        } else {
          Json.obj(
            "movementReferenceNumber" -> mrn,
            "rejectionDate" -> rejectionDate,
            "action" -> action,
            "reason" -> reason,
            "errors" -> Json.toJson(errors)
          )
        }

        val rejection = ArrivalNotificationRejection(mrn, rejectionDate, Some(action), Some(reason), errors)

        Json.toJson(rejection) mustEqual json
    }
  }
}
