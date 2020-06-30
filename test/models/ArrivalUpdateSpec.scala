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

import java.time.LocalDateTime

import base.FreeSpecDiscipline
import base.SpecBase
import cats._
import cats.implicits._
import cats.kernel.laws.discipline.SemigroupTests
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

class ArrivalUpdateSpec
    extends SpecBase
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with ModelGenerators
    with FreeSpecDiscipline
    with MongoDateTimeFormats {

  val arrivalStatusUpdate: Gen[ArrivalUpdate] =
    for {
      arrivalUpdate <- arbitrary[ArrivalUpdate]
      arrivalStatus <- arbitrary[ArrivalStatus]
    } yield arrivalUpdate.copy(arrivalUpdate = Some(arrivalStatus))

  val messageStatusUpdate =
    for {
      arrivalUpdate <- arbitrary[ArrivalUpdate]
      messageStatus <- arbitrary[MessageStatusUpdate]
    } yield arrivalUpdate.copy(messageUpdate = Some(messageStatus))

  def noArrivalUpdate(gen: Gen[ArrivalUpdate] = arbitrary[ArrivalUpdate]): Gen[ArrivalUpdate] =
    gen.map(_.copy(arrivalUpdate = None))

  def noMessageUpdate(gen: Gen[ArrivalUpdate] = arbitrary[ArrivalUpdate]): Gen[ArrivalUpdate] =
    gen.map(_.copy(messageUpdate = None))

  implicit val eqArrivalStatusUpdate: Eq[ArrivalUpdate] =
    new Eq[ArrivalUpdate] {
      override def eqv(x: ArrivalUpdate, y: ArrivalUpdate): Boolean = x == y
    }

  "ArrivalUpdate" - {

    "combine" - {
      checkAll("Semigroup behaviour", SemigroupTests[ArrivalUpdate].semigroup)

      "uses the arrival status on the right when both are defined" in {
        forAll(arrivalStatusUpdate, arrivalStatusUpdate) {
          (lhs, rhs) =>
            lhs.combine(rhs).arrivalUpdate mustEqual rhs.arrivalUpdate
        }
      }

      "uses the arrival status on the left when the right is not are defined" in {
        forAll(arrivalStatusUpdate, noArrivalUpdate()) {
          (lhs, rhs) =>
            lhs.combine(rhs).arrivalUpdate mustEqual lhs.arrivalUpdate
        }
      }

      "is not defined when both arrival status are not defined" in {
        forAll(noArrivalUpdate(), noArrivalUpdate()) {
          (lhs, rhs) =>
            lhs.combine(rhs).arrivalUpdate must not be (defined)
        }

      }

      "uses the message status on the right when both are defined" in {
        forAll(messageStatusUpdate, messageStatusUpdate) {
          (lhs, rhs) =>
            lhs.combine(rhs).messageUpdate mustEqual rhs.messageUpdate
        }
      }

      "uses the message status on the left when the right is not are defined" in {
        forAll(messageStatusUpdate, noMessageUpdate()) {
          (lhs, rhs) =>
            lhs.combine(rhs).messageUpdate mustEqual lhs.messageUpdate
        }
      }

      "is not defined when both message status are not defined" in {
        forAll(noMessageUpdate(), noMessageUpdate()) {
          (lhs, rhs) =>
            lhs.combine(rhs).messageUpdate must not be (defined)
        }
      }

      "uses the arrival status and the message status when they are defined in different ArrivalUpdates" - {
        "arrivalUpdate + messageUpdate" in {
          forAll(noMessageUpdate(arrivalStatusUpdate), noArrivalUpdate(messageStatusUpdate)) {
            (arrivalUpdate, messageUpdate) =>
              val leftRightResult = arrivalUpdate.combine(messageUpdate)

              leftRightResult.arrivalUpdate mustEqual arrivalUpdate.arrivalUpdate
              leftRightResult.messageUpdate mustEqual messageUpdate.messageUpdate
          }
        }

        "messageUpdate + arrivalUpdate" in {
          forAll(noMessageUpdate(arrivalStatusUpdate), noArrivalUpdate(messageStatusUpdate)) {
            (arrivalUpdate, messageUpdate) =>
              val rightLeftResult = messageUpdate.combine(arrivalUpdate)

              rightLeftResult.arrivalUpdate mustEqual arrivalUpdate.arrivalUpdate
              rightLeftResult.messageUpdate mustEqual messageUpdate.messageUpdate
          }
        }
      }
    }

    "ArrivalStatusUpdate" - {
      "the update must only be the arrival state and the value must be the name of the state" in {
        forAll(noMessageUpdate(arrivalStatusUpdate)) {
          arrivalUpdate =>
            val expectedJson = Json.obj(
              "$set" -> Json.obj(
                "status"      -> arrivalUpdate.arrivalUpdate.value.toString,
                "lastUpdated" -> LocalDateTime.now.withSecond(0).withNano(0)
              )
            )

            ArrivalModifier.toJson(arrivalUpdate) mustEqual expectedJson

        }
      }
      "the update must only be the message update and the value must be the name of the message" in {
        forAll(noArrivalUpdate(messageStatusUpdate)) {
          arrivalUpdate =>
            val messageId = arrivalUpdate.messageUpdate.value.messageId
            val expectedJson = Json.obj(
              "$set" -> Json.obj(
                s"messages.${messageId.index}.status" -> arrivalUpdate.messageUpdate.value.messageStatus,
                "lastUpdated"                         -> LocalDateTime.now.withSecond(0).withNano(0)
              )
            )
            ArrivalModifier.toJson(arrivalUpdate) mustEqual expectedJson
        }
      }
      "the update must be arrival update, message update and the value must be both" in {
        forAll(generatorArrivalUpdate) {
          arrivalUpdate =>
            val messageId = arrivalUpdate.messageUpdate.value.messageId
            val expectedJson = Json.obj(
              "$set" -> Json.obj(
                "lastUpdated"                         -> LocalDateTime.now.withSecond(0).withNano(0),
                "status"                              -> arrivalUpdate.arrivalUpdate.value.toString,
                s"messages.${messageId.index}.status" -> arrivalUpdate.messageUpdate.value.messageStatus
              )
            )
            ArrivalModifier.toJson(arrivalUpdate) mustEqual expectedJson
        }
      }
    }
  }
}
