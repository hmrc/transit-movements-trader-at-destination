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

package services.testOnly

import base.SpecBase
import models.ArrivalId
import models.MovementReferenceNumber
import models.testOnly.SeedDataParameters
import models.testOnly.SeedEori
import models.testOnly.SeedMrn

import java.time.Clock
import java.time.Instant
import java.time.ZoneId

class TestOnlySeedDataServiceSpec extends SpecBase {

  val testClock = Clock.fixed(Instant.now(), ZoneId.systemDefault)

  "seedArrivals" - {

    "returns an iterator of arrivals with Arrival Id starting at ??? for the number of users and movements specified" in {

      val seedEori  = SeedEori("ZZ", 1, 12)
      val seedEori1 = SeedEori("ZZ", 2, 12)
      val seedMrn   = SeedMrn("21GB", 1, 14)
      val seedMrn1  = SeedMrn("21GB", 2, 14)

      val seedDataParameters = SeedDataParameters(seedEori, 2, seedMrn, 2)

      val expectedResult = Seq(
        (ArrivalId(Int.MaxValue - 3), seedEori.format, MovementReferenceNumber(seedMrn.format)),
        (ArrivalId(Int.MaxValue - 2), seedEori.format, MovementReferenceNumber(seedMrn1.format)),
        (ArrivalId(Int.MaxValue - 1), seedEori1.format, MovementReferenceNumber(seedMrn.format)),
        (ArrivalId(Int.MaxValue), seedEori1.format, MovementReferenceNumber(seedMrn1.format))
      )

      val result =
        TestOnlySeedDataService
          .seedArrivals(seedDataParameters, testClock)
          .toSeq
          .map(x => (x.arrivalId, x.eoriNumber, x.movementReferenceNumber))

      result mustBe expectedResult
    }
  }

}
