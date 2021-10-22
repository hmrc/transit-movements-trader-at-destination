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

import base.SpecBase
import generators.ModelGenerators
import models.MessageType._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json._

class MessageTypeSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "Json reads and writes" - {
    "writes" in {
      forAll(arbitrary[MessageType]) {
        messageType =>
          Json.toJson(messageType) mustEqual JsString(messageType.code)
      }
    }

    "reads" - {
      "returns the message type when given the code for a message" in {
        forAll(arbitrary[MessageType]) {
          message =>
            JsString(message.code).validate[MessageType] mustEqual JsSuccess(message)
        }
      }

      "returns an error when the message type code is not recognised" in {
        val invalidMessageCode = JsString("InvalidMessageCode")

        invalidMessageCode.validate[MessageType] mustEqual JsError("Not a recognised value")
      }

      "returns an error when the message type code is not a string" in {
        val invalidMessageCode = JsNumber(1)

        invalidMessageCode.validate[MessageType] mustEqual JsError("Invalid type. Expected a JsString got a class play.api.libs.json.JsNumber")
      }

    }

    "ordering" - {
      "comparing to ArrivalNotification" - {

        "all status must have greater order" in {

          messageTypesExcluding(ArrivalNotification).foreach {
            status =>
              val result = Ordering[MessageType].max(ArrivalNotification, status)

              result mustBe status
          }
        }
      }

      "comparing to XMLSubmissionNegativeAcknowledgement" - {

        val lesserOrderValues = Seq(
          ArrivalNotification,
          UnloadingRemarks
        )

        val greaterOrderValues = Seq(
          XMLSubmissionNegativeAcknowledgement,
          UnloadingPermission,
          UnloadingRemarksRejection,
          GoodsReleased
        )

        "is greater order than ArrivalNotification and UnloadingRemarksRejection" in {

          forAll(Gen.oneOf(lesserOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(XMLSubmissionNegativeAcknowledgement, status)

              result mustBe XMLSubmissionNegativeAcknowledgement
          }
        }

        "in lesser order than any other status" in {

          forAll(Gen.oneOf(greaterOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(XMLSubmissionNegativeAcknowledgement, status)

              result mustBe status
          }
        }
      }

      "comparing to UnloadingRemarksRejection" - {

        val lesserOrderValues = Seq(
          ArrivalNotification,
          ArrivalRejection,
          UnloadingPermission,
          UnloadingRemarks,
          XMLSubmissionNegativeAcknowledgement
        )

        val greaterOrderValues = Seq(
          GoodsReleased,
          UnloadingRemarksRejection
        )

        "is greater order than ArrivalNotification, ArrivalRejection, UnloadingPermission and UnloadingRemarks" in {

          forAll(Gen.oneOf(lesserOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(UnloadingRemarksRejection, status)

              result mustBe UnloadingRemarksRejection
          }
        }

        "in lesser order than any other status" in {

          forAll(Gen.oneOf(greaterOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(UnloadingRemarksRejection, status)

              result mustBe status
          }
        }
      }

      "comparing to UnloadingPermission" - {

        val lesserOrderValues = Seq(
          ArrivalNotification,
          ArrivalRejection,
          XMLSubmissionNegativeAcknowledgement
        )

        val greaterOrderValues = Seq(
          UnloadingPermission,
          UnloadingRemarks,
          GoodsReleased,
          UnloadingRemarksRejection
        )

        "is greater order than ArrivalNotification and ArrivalRejection" in {

          forAll(Gen.oneOf(lesserOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(UnloadingPermission, status)

              result mustBe UnloadingPermission
          }
        }

        "in lesser order than any other status" in {

          forAll(Gen.oneOf(greaterOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(UnloadingPermission, status)

              result mustBe status
          }
        }
      }

      "comparing to UnloadingRemarks" - {

        val lesserOrderValues = Seq(
          ArrivalNotification,
          UnloadingPermission,
          ArrivalRejection
        )

        val greaterOrderValues = Seq(
          XMLSubmissionNegativeAcknowledgement,
          UnloadingRemarks,
          GoodsReleased,
          UnloadingRemarksRejection
        )

        "is greater order than ArrivalNotification, UnloadingPermission, ArrivalRejection" in {

          forAll(Gen.oneOf(lesserOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(UnloadingRemarks, status)

              result mustBe UnloadingRemarks
          }
        }

        "in lesser order than any other status" in {

          forAll(Gen.oneOf(greaterOrderValues)) {
            status =>
              val result = Ordering[MessageType].max(UnloadingRemarks, status)

              result mustBe status
          }
        }
      }

      "comparing to GoodsReleased" - {

        "all status must be lesser order" in {

          messageTypesExcluding(GoodsReleased).foreach {
            status =>
              val result = Ordering[MessageType].max(GoodsReleased, status)

              result mustBe GoodsReleased
          }
        }
      }
    }
  }

  def messageTypesExcluding(exclude: MessageType*): Seq[MessageType] =
    MessageType.values.filterNot(
      x => exclude.toSet.contains(x)
    )
}
