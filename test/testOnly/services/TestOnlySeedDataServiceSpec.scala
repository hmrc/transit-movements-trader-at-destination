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

package testOnly.services

import base.SpecBase
import cats.data.NonEmptyList
import models.ArrivalId
import models.MovementMessage
import models.MovementReferenceNumber
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import testOnly.models.SeedDataParameters
import testOnly.models.SeedEori
import testOnly.models.SeedMrn

import java.time.Clock
import java.time.Instant
import java.time.ZoneId

class TestOnlySeedDataServiceSpec extends SpecBase with ScalaCheckDrivenPropertyChecks {
  implicit def dontShrink[A]: Shrink[A] = Shrink.shrinkAny

  val testClock = Clock.fixed(Instant.now(), ZoneId.systemDefault)

  "seedArrivals" - {

    "returns an iterator of arrivals with Arrival Id's for the number of users and movements specified" in {

      val seedEori  = SeedEori("ZZ", 1, 12)
      val seedEori1 = SeedEori("ZZ", 2, 12)
      val seedMrn   = SeedMrn("21GB", 1, 14)
      val seedMrn1  = SeedMrn("21GB", 2, 14)

      val seedDataParameters = SeedDataParameters(2, 2, ArrivalId(0), None)

      val expectedResult = Seq(
        (ArrivalId(0), seedEori.format, MovementReferenceNumber(seedMrn.format)),
        (ArrivalId(1), seedEori.format, MovementReferenceNumber(seedMrn1.format)),
        (ArrivalId(2), seedEori1.format, MovementReferenceNumber(seedMrn.format)),
        (ArrivalId(3), seedEori1.format, MovementReferenceNumber(seedMrn1.format))
      )

      val result =
        TestOnlySeedDataService
          .seedArrivals(seedDataParameters, testClock)
          .toSeq
          .map(x => (x.arrivalId, x.eoriNumber, x.movementReferenceNumber))

      result mustBe expectedResult
    }

    "returns an iterator of arrivals with dynamic xml" in {

      val seedDataParameters = SeedDataParameters(2, 2, ArrivalId(0), None)

      val expectedEori1 = SeedEori("ZZ", 1, 12)
      val expectedEori2 = SeedEori("ZZ", 2, 12)
      val expectedMrn1  = SeedMrn("21GB", 1, 14)
      val expectedMrn2  = SeedMrn("21GB", 2, 14)

      val result: Seq[NonEmptyList[MovementMessage]] =
        TestOnlySeedDataService
          .seedArrivals(seedDataParameters, testClock)
          .toSeq
          .map(_.messages)

      result.length mustBe 4

      (result.head.head.message \\ "TRADESTRD" \\ "TINTRD59").text mustBe expectedEori1.format
      (result.head.head.message \\ "HEAHEA" \\ "DocNumHEA5").text mustBe expectedMrn1.format

      (result(1).head.message \\ "TRADESTRD" \\ "TINTRD59").text mustBe expectedEori1.format
      (result(1).head.message \\ "HEAHEA" \\ "DocNumHEA5").text mustBe expectedMrn2.format

      (result(2).head.message \\ "TRADESTRD" \\ "TINTRD59").text mustBe expectedEori2.format
      (result(2).head.message \\ "HEAHEA" \\ "DocNumHEA5").text mustBe expectedMrn1.format

      (result(3).head.message \\ "TRADESTRD" \\ "TINTRD59").text mustBe expectedEori2.format
      (result(3).head.message \\ "HEAHEA" \\ "DocNumHEA5").text mustBe expectedMrn2.format
    }

    "the total number of Arrivals from the iterator is the (Number users x number of movements)" in {
      val genInt = Gen.choose(10, 50)

      forAll(genInt, genInt) {
        (eoriCount, mrnCount) =>
          val seedDataParameters = SeedDataParameters(eoriCount, mrnCount, ArrivalId(0), None)

          val iterator = TestOnlySeedDataService.seedArrivals(seedDataParameters, testClock)

          iterator.length mustEqual (eoriCount * mrnCount)
      }
    }
  }

}
