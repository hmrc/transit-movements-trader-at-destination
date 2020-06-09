/*
 * Copyright 2020 HM Revenue & Customs
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

import base.FreeSpecDiscipline
import base.SpecBase
import cats._
import cats.implicits._
import cats.kernel.laws.discipline.SemigroupTests
import generators.ModelGenerators
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.Json

class ArrivalUpdateSpec extends SpecBase with Matchers with ScalaCheckDrivenPropertyChecks with ModelGenerators with FreeSpecDiscipline {

  implicit val eqArrivalStatusUpdate: Eq[ArrivalUpdate] = _ == _

  "ArrivalUpdate" - {

//    "combine" - {
//
//      checkAll("Semigroup behaviour", SemigroupTests[ArrivalUpdate].semigroup)
//
//      "uses the arrival status on the right when both are defined" in {
//        forAll(???, ???) {
//          (lhs, rhs) =>
//            lhs.combine(rhs).arrivalUpdate mustEqual rhs.arrivalUpdate
//        }
//      }
//
//      "uses the arrival status on the left when the right is not are defined" in {
//        forAll(???, ???) {
//          (lhs, rhs) =>
//            lhs.combine(rhs).arrivalUpdate mustEqual lhs.arrivalUpdate
//        }
//      }
//
//      "is not defined when both arrival status are not defined" in {
//        forAll(???, ???) {
//          (lhs, rhs) =>
//            lhs.combine(rhs).arrivalUpdate must not be (defined)
//        }
//
//      }
//
//      "uses the message status on the right when both are defined" in {
//        forAll(???, ???) {
//          (lhs, rhs) =>
//            lhs.combine(rhs).messageUpdate mustEqual rhs.messageUpdate
//        }
//      }
//
//      "uses the message status on the left when the right is not are defined" in {
//        forAll(???, ???) {
//          (lhs, rhs) =>
//            lhs.combine(rhs).messageUpdate mustEqual lhs.messageUpdate
//        }
//      }
//
//      "is not defined when both message status are not defined" in {
//        forAll(???, ???) {
//          (lhs, rhs) =>
//            lhs.combine(rhs).messageUpdate must not be (defined)
//        }
//      }
//
//      "uses the arrival status and the message status when they are defined in different ArrivalUpdates" - {
//        "arrivalUpdate + messageUpdate" in {
//          forAll(???, ???) {
//            (arrivalUpdate, messageUpdate) =>
//              val leftRightResult = arrivalUpdate.combine(messageUpdate)
//
//              leftRightResult.arrivalUpdate mustEqual arrivalUpdate.arrivalUpdate
//              leftRightResult.messageUpdate mustEqual messageUpdate.messageUpdate
//          }
//        }
//
//        "messageUpdate + arrivalUpdate" in {
//          forAll(???, ???) {
//            (arrivalUpdate, messageUpdate) =>
//              val rightLeftResult = messageUpdate.combine(arrivalUpdate)
//
//              rightLeftResult.arrivalUpdate mustEqual arrivalUpdate.arrivalUpdate
//              rightLeftResult.messageUpdate mustEqual messageUpdate.messageUpdate
//          }
//        }
//      }
//    }
//
//    "ArrivalStatusUpdate" - {
//      "the update must only be the arrival state and the value must be the name of the state" in {
//        forAll(???) {
//          arrivalUpdate =>
//            val expectedJson = Json.obj(
//              "$set" -> Json.obj(
//                "status" -> arrivalUpdate.arrivalUpdate.value.toString
//              )
//            )
//
//            ArrivalModifier.toJson(arrivalUpdate) mustEqual expectedJson
//
//        }
//      }
//      "the update must only be the message update and the value must be the name of the message" in {
//        forAll(???) {
//          arrivalUpdate =>
//            val messageId = arrivalUpdate.messageUpdate.value.messageId
//            val expectedJson = Json.obj(
//              "$set" -> Json.obj(
//                s"messages.${messageId.index}.status" -> arrivalUpdate.messageUpdate.value.messageStatus
//              )
//            )
//            ArrivalModifier.toJson(arrivalUpdate) mustEqual expectedJson
//        }
//      }
//      "the update must be arrival update, message update and the value must be both" in {
//        forAll(???) {
//          arrivalUpdate =>
//            val messageId = arrivalUpdate.messageUpdate.value.messageId
//            val expectedJson = Json.obj(
//              "$set" -> Json.obj(
//                "status"                              -> arrivalUpdate.arrivalUpdate.value.toString,
//                s"messages.${messageId.index}.status" -> arrivalUpdate.messageUpdate.value.messageStatus
//              )
//            )
//            ArrivalModifier.toJson(arrivalUpdate) mustEqual expectedJson
//        }
//      }
//    }
  }
}
