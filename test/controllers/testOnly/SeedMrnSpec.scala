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

class SeedMrnSpec extends SpecBase {

  "SeedMrn" - {

    "must read from valid value" in {

      val json = Json.toJson("21GB00000000000001")

      json.as[SeedMrn] mustEqual SeedMrn("21GB", 1, 14)
    }

    "must write from valid value in correct format" in {

      val json  = Json.toJson("21GB00000000000001")
      val json2 = Json.toJson("21GB12345678901234")

      val seedMrn  = SeedMrn("21GB", 1, 14)
      val seedMrn2 = SeedMrn("21GB", 12345678901234L, 1)

      Json.toJson(seedMrn) mustEqual json
      Json.toJson(seedMrn2) mustEqual json2
    }
  }

}
