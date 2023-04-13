/*
 * Copyright 2023 HM Revenue & Customs
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

package models.behaviours

import generators.ModelGenerators
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes

trait JsonBehaviours extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators {

  class DualReadsAndWrites[A] {

    def apply(gen: Gen[A])(implicit ev1: Reads[A], ev2: Writes[A]): Unit =
      "must have dual reads and writes" in {

        forAll(gen) {
          model =>
            val json = Json.toJson(model)

            json.validate[A] mustEqual JsSuccess(model)
        }
      }
  }

  def mustHaveDualReadsAndWrites[A]: DualReadsAndWrites[A] = new DualReadsAndWrites[A]
}
