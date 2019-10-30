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

import java.time.LocalDateTime

import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.Format

class InterchangeControlReferenceSpec extends FreeSpec with MustMatchers
  with ScalaCheckPropertyChecks with ModelGenerators {

  "InterchangeControlReference" - {

    "must toString in the correct format" in {

      val localDateTime: Gen[LocalDateTime] = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)

      forAll(arbitrary[String], localDateTime) {
        (prefix, dateTime) =>

          InterchangeControlReference(prefix, dateTime).toString mustBe s"$prefix${Format.dateFormatted(dateTime)}-1"
      }
    }
  }

}
