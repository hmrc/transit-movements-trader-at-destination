/*
 * Copyright 2022 HM Revenue & Customs
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

package testOnly.models

import base.SpecBase
import models.ArrivalId
import models.ChannelType
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SeedDataParametersSpec extends SpecBase with ScalaCheckDrivenPropertyChecks {

  implicit def dontShrink[A]: Shrink[A] = Shrink.shrinkAny

  implicit val orderingSeedEori: Ordering[SeedEori] =
    Ordering.fromLessThan[SeedEori]((x, y) => x.suffix < y.suffix)

  implicit val orderingArrivalId: Ordering[ArrivalId] =
    Ordering.fromLessThan[ArrivalId]((x, y) => x.index < y.index)

  implicit val orderingSeedMrn: Ordering[SeedMrn] =
    Ordering.fromLessThan[SeedMrn]((x, y) => x.suffix < y.suffix)

  "SeedDataParameters" - {
    "seedData" - {
      "eori values" - {
        "there must be the same number of eori values as the number configured" in {
          forAll(Gen.chooseNum(1, 100)) {
            number =>
              val startEori = SeedEori("ZZ", 11, 12)

              val seedDataParameters = SeedDataParameters(number, 2, ArrivalId(0), Some(startEori), Some(ChannelType.web))

              val result = seedDataParameters.seedData.map(_._2).toSet

              result.size mustEqual number
          }
        }

        "the values of the eori must be increasing from the start value provided" in {
          val startEori = SeedEori("ZZ", 11, 12)

          val seedDataParameters = SeedDataParameters(2, 2, ArrivalId(0), Some(startEori), Some(ChannelType.web))

          val result = seedDataParameters.seedData.map(_._2).toList

          result mustBe sorted
        }

        "the values of the eori" - {
          "when there is a start eori specified" in {
            val startEori = SeedEori("ZZ", 11, 12)

            val seedDataParameters = SeedDataParameters(3, 2, ArrivalId(0), Some(startEori), Some(ChannelType.web))

            val result = seedDataParameters.seedData.map(_._2).toList

            result must contain inOrderOnly (SeedEori("ZZ", 11, 12), SeedEori("ZZ", 12, 12), SeedEori("ZZ", 13, 12))
          }

          "when no start eori is specified" in {

            val seedDataParameters = SeedDataParameters(3, 2, ArrivalId(0), None, None)

            val result = seedDataParameters.seedData.map(_._2).toList

            result must contain inOrderOnly (SeedEori("ZZ", 1, 12), SeedEori("ZZ", 2, 12), SeedEori("ZZ", 3, 12))
          }

        }
      }

      "ArrivalId values" - {
        "there must be the same number as the configure number of movements" in {
          forAll(Gen.chooseNum(1, 100)) {
            number =>
              val seedDataParameters = SeedDataParameters(number, 2, ArrivalId(0), None, None)

              val result = seedDataParameters.seedData.map(_._1).toSet

              result.size mustEqual seedDataParameters.numberOfMovements
          }
        }

        "the values of the ArrivalId must be increasing from the start value provided" in {
          val seedDataParameters = SeedDataParameters(2, 2, ArrivalId(0), None, None)

          val result = seedDataParameters.seedData.map(_._2).toList

          result mustBe sorted
        }

        "the values of the ArrivalId must have a difference of 1" in {

          val seedDataParameters = SeedDataParameters(3, 2, ArrivalId(101), None, None)

          val result = seedDataParameters.seedData.map(_._1).toList

          val expectedArrivalIds = List(
            ArrivalId(101),
            ArrivalId(102),
            ArrivalId(103),
            ArrivalId(104),
            ArrivalId(105),
            ArrivalId(106)
          )

          result must contain theSameElementsInOrderAs (expectedArrivalIds)
        }
      }

      "mrn values" - {
        "the values of the must be increasing from the default start mrn" in {
          forAll(Gen.chooseNum(1, 100)) {
            numberOfMrns =>
              val seedDataParameters = SeedDataParameters(2, numberOfMrns, ArrivalId(0), None, None)

              val result = seedDataParameters.seedData.map(_._3).take(numberOfMrns).toList

              result mustBe sorted
          }
        }

        "repeat for each EORI" in {
          val movementsPerUser   = 3
          val seedDataParameters = SeedDataParameters(2, movementsPerUser, ArrivalId(0), None, None)

          val result = seedDataParameters.seedData.map(x => (x._2, x._3)).toList

          val expected = List(
            (SeedEori("ZZ", 1, 12), SeedMrn("21GB", 1, 14)),
            (SeedEori("ZZ", 1, 12), SeedMrn("21GB", 2, 14)),
            (SeedEori("ZZ", 1, 12), SeedMrn("21GB", 3, 14)),
            (SeedEori("ZZ", 2, 12), SeedMrn("21GB", 1, 14)),
            (SeedEori("ZZ", 2, 12), SeedMrn("21GB", 2, 14)),
            (SeedEori("ZZ", 2, 12), SeedMrn("21GB", 3, 14))
          )

          result must contain theSameElementsInOrderAs (expected)
        }
      }
    }
  }

}
