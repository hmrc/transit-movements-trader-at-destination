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

package controllers.testOnly

import base.SpecBase
import play.api.libs.json.Json

class SeedEoriSpec extends SpecBase {

  "SeedEori" - {

    "must read from valid value" in {

      val json = Json.toJson("ZZ000000000001")

      json.as[SeedEori] mustEqual SeedEori("ZZ", 1, 12)
    }

    "must write from valid value in correct format" in {

      val json  = Json.toJson("ZZ000000000001")
      val json2 = Json.toJson("GB602070107000")

      val seedEori  = SeedEori("ZZ", 1, 12)
      val seedEori2 = SeedEori("GB", 602070107000L, 1)

      Json.toJson(seedEori) mustEqual json
      Json.toJson(seedEori2) mustEqual json2
    }
  }

}
