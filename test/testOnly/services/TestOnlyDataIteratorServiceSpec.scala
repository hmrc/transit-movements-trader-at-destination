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
import testOnly.models.SeedDataParameters
import testOnly.models.SeedEori
import testOnly.models.SeedMrn

class TestOnlyDataIteratorServiceSpec extends SpecBase {

  "seedDataIterator" - {

    "must create an iteration of eori's and mrn's" in {

      val seedEori  = SeedEori("ZZ", 1, 12)
      val seedEori1 = SeedEori("ZZ", 2, 12)

      val seedMrn  = SeedMrn("21GB", 1, 14)
      val seedMrn1 = SeedMrn("21GB", 2, 14)

      val seedDataParameters = SeedDataParameters(seedEori, 2, seedMrn, 2)

      val expectedResult = Seq(
        (seedEori, seedMrn),
        (seedEori, seedMrn1),
        (seedEori1, seedMrn),
        (seedEori1, seedMrn1)
      )

      TestOnlyDataIteratorService.seedDataIterator(seedDataParameters).toSeq mustBe expectedResult
    }
  }

}
