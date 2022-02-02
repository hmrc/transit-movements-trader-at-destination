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

package models

import java.time.LocalDateTime
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class MovementMessageSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators {

  "MovementMessageWithStatus" - {

    ".reads" - {

      "must succeed when the json contains all of the nodes" in {

        forAll(arbitrary[MessageId], arbitrary[LocalDateTime], arbitrary[MessageType], arbitrary[MessageStatus], arbitrary[Int]) {
          case (messageId, dateTime, messageType, status, messageCorrelationId) =>
            val xml = <xml><node1>foo</node1><node2>bar</node2></xml>

            val json = Json.obj(
              "messageId"            -> Json.toJson(messageId),
              "dateTime"             -> Json.toJson(dateTime)(MongoDateTimeFormats.localDateTimeWrite),
              "messageType"          -> Json.toJson(messageType),
              "message"              -> xml.toString,
              "status"               -> Json.toJson(status),
              "messageCorrelationId" -> messageCorrelationId
            )

            val expectedMovementMessage = MovementMessageWithStatus(messageId, dateTime, messageType, xml, status, messageCorrelationId)

            json.validate[MovementMessageWithStatus] mustEqual JsSuccess(expectedMovementMessage)
        }
      }

      "must succeed when the json does not contain the `messageJson` node, setting that field to an empty Json object" in {

        forAll(arbitrary[MessageId], arbitrary[LocalDateTime], arbitrary[MessageType], arbitrary[MessageStatus], arbitrary[Int]) {
          case (messageId, dateTime, messageType, status, messageCorrelationId) =>
            val xml = <xml><node1>foo</node1><node2>bar</node2></xml>

            val json = Json.obj(
              "messageId"            -> Json.toJson(messageId),
              "dateTime"             -> Json.toJson(dateTime)(MongoDateTimeFormats.localDateTimeWrite),
              "messageType"          -> Json.toJson(messageType),
              "message"              -> xml.toString,
              "status"               -> Json.toJson(status),
              "messageCorrelationId" -> messageCorrelationId
            )

            val expectedMovementMessage = MovementMessageWithStatus(messageId, dateTime, messageType, xml, status, messageCorrelationId)

            json.validate[MovementMessageWithStatus] mustEqual JsSuccess(expectedMovementMessage)
        }
      }
    }
  }

  "MovementMessageWithoutStatus" - {

    ".reads" - {

      "must succeed when the json contains all of the nodes" in {

        forAll(arbitrary[MessageId], arbitrary[LocalDateTime], arbitrary[MessageType], arbitrary[Int]) {
          case (messageId, dateTime, messageType, messageCorrelationId) =>
            val xml = <xml><node1>foo</node1><node2>bar</node2></xml>

            val json = Json.obj(
              "messageId"            -> Json.toJson(messageId),
              "dateTime"             -> Json.toJson(dateTime)(MongoDateTimeFormats.localDateTimeWrite),
              "messageType"          -> Json.toJson(messageType),
              "message"              -> xml.toString,
              "messageCorrelationId" -> messageCorrelationId
            )

            val expectedMovementMessage = MovementMessageWithoutStatus(messageId, dateTime, messageType, xml, messageCorrelationId)

            json.validate[MovementMessageWithoutStatus] mustEqual JsSuccess(expectedMovementMessage)
        }
      }

      "must succeed when the json does not contain the `messageJson` node, setting that field to an empty Json object" in {

        forAll(arbitrary[MessageId], arbitrary[LocalDateTime], arbitrary[MessageType], arbitrary[Int]) {
          case (messageId, dateTime, messageType, messageCorrelationId) =>
            val xml = <xml><node1>foo</node1><node2>bar</node2></xml>

            val json = Json.obj(
              "messageId"            -> Json.toJson(messageId),
              "dateTime"             -> Json.toJson(dateTime)(MongoDateTimeFormats.localDateTimeWrite),
              "messageType"          -> Json.toJson(messageType),
              "message"              -> xml.toString,
              "messageCorrelationId" -> messageCorrelationId
            )

            val expectedMovementMessage = MovementMessageWithoutStatus(messageId, dateTime, messageType, xml, messageCorrelationId)

            json.validate[MovementMessageWithoutStatus] mustEqual JsSuccess(expectedMovementMessage)
        }
      }
    }
  }
}
