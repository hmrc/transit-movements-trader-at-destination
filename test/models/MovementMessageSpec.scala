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

package models

import java.time.LocalDateTime

import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class MovementMessageSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators {

  "MovementMessageWithStatus" - {

    ".apply" - {

      "must create a MovementMessageWithStatus that includes a JSON representation of the XML message" in {

        forAll(arbitrary[LocalDateTime], arbitrary[MessageType], arbitrary[MessageStatus], arbitrary[Int]) {
          case (dateTime, messageType, status, messageCorrelationId) =>
            val xml = <xml><node1>foo</node1><node2>bar</node2></xml>
            val expectedJson = {
              Json.obj(
                "xml" ->
                  Json.obj(
                    "node1" -> "foo",
                    "node2" -> "bar"
                  ))
            }

            val modelFromApply        = MovementMessageWithStatus(dateTime, messageType, xml, status, messageCorrelationId)
            val modelFromConstruction = MovementMessageWithStatus(dateTime, messageType, xml, status, messageCorrelationId, expectedJson)

            modelFromApply mustEqual modelFromConstruction
        }
      }
    }
  }

  "MovementMessageWithoutStatus" - {

    ".apply" - {

      "must create a MovementMessageWithoutStatus that includes a JSON representation of the XML message" in {

        forAll(arbitrary[LocalDateTime], arbitrary[MessageType], arbitrary[Int]) {
          case (dateTime, messageType, messageCorrelationId) =>
            val xml = <xml><node1>foo</node1><node2>bar</node2></xml>
            val expectedJson = {
              Json.obj(
                "xml" ->
                  Json.obj(
                    "node1" -> "foo",
                    "node2" -> "bar"
                  ))
            }

            val modelFromApply        = MovementMessageWithoutStatus(dateTime, messageType, xml, messageCorrelationId)
            val modelFromConstruction = MovementMessageWithoutStatus(dateTime, messageType, xml, messageCorrelationId, expectedJson)

            modelFromApply mustEqual modelFromConstruction
        }
      }
    }
  }
}
