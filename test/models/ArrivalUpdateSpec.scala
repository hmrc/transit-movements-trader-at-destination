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
import java.time.Clock
import java.time.ZoneOffset

import base.FreeSpecDiscipline
import base.SpecBase
import cats._
import cats.kernel.laws.discipline.SemigroupTests
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.Json

class ArrivalUpdateSpec
    extends SpecBase
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with ModelGenerators
    with FreeSpecDiscipline
    with MongoDateTimeFormats {

  implicit val eqArrivalStatusUpdate: Eq[ArrivalUpdate] = _ == _
  val currentDateTime                                   = LocalDateTime.now.withSecond(0).withNano(0).toInstant(ZoneOffset.UTC)
  implicit val clock: Clock                             = Clock.fixed(currentDateTime, ZoneOffset.UTC)

  "ArrivalUpdate" - {

    "combine" - {

      checkAll("Semigroup behaviour", SemigroupTests[ArrivalUpdate].semigroup)

      "returns the value when it is combined with itself" in {
        forAll(arbitrary[ArrivalUpdate]) {
          arrivalUpdate =>
            Semigroup[ArrivalUpdate].combine(arrivalUpdate, arrivalUpdate) mustEqual arrivalUpdate
        }
      }

      "returns the value on the right when it is the same type of update" in {
        forAll(arrivalUpdateTypeGenerator) {
          arrivalUpdateupdateGenerator =>
            forAll(arrivalUpdateupdateGenerator, arrivalUpdateupdateGenerator) {
              (lhs, rhs) =>
                Semigroup[ArrivalUpdate].combine(lhs, rhs) mustEqual rhs
            }
        }
      }
    }

    "ArrivalModifier for any combination of ArrivalUpdate is the same as the individual ArrivalModifier combined" - {
      "when combined with an ArrivalStatusUpdate" in {
        forAll(arbitrary[ArrivalUpdate], arbitrary[ArrivalStatusUpdate]) {
          (lhs, rhs) =>
            val result = Semigroup[ArrivalUpdate].combine(lhs, rhs)

            val expectedValue = ArrivalModifier.toJson(lhs) deepMerge ArrivalModifier.toJson(rhs)

            ArrivalModifier.toJson(result) mustEqual expectedValue

        }
      }

      "when combined with an update that updates a message" in {
        def removeMessageUpdate(json: JsObject): JsObject = {
          val updateDescription = (json \ "$set").toOption.value
            .asInstanceOf[JsObject]
            .fields
            .filterNot(_._1.contains("messages."))

          Json.obj("$set" -> JsObject(updateDescription))
        }

        val updatesWithMessage = Gen.oneOf(
          arbitrary[MessageStatusUpdate],
          arbitrary[CompoundStatusUpdate],
          arbitrary[ArrivalPutUpdate]
        )

        forAll(arbitrary[ArrivalUpdate], updatesWithMessage) {
          (lhs, rhs: ArrivalUpdate) =>
            val result = Semigroup[ArrivalUpdate].combine(lhs, rhs)

            val expectedValue =
              removeMessageUpdate(ArrivalModifier.toJson(lhs)) deepMerge ArrivalModifier.toJson(rhs)

            ArrivalModifier.toJson(result) mustEqual expectedValue

        }

      }
    }

  }

  "MessageStatusUpdate" - {
    "ArrivalModifier returns modify object that would set the message status" in {
      forAll(arbitrary[MessageStatusUpdate]) {
        messageStatusUpdate =>
          val expectedUpdateJson = Json.obj(
            "$set" -> Json.obj(
              s"messages.${messageStatusUpdate.messageId.index}.status" -> messageStatusUpdate.messageStatus,
              "lastUpdated"                                             -> LocalDateTime.now(clock).withSecond(0).withNano(0)
            )
          )

          ArrivalModifier.toJson(messageStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }

  "ArrivalStatusUpdate" - {
    "ArrivalModifier returns modify object that would set the status" in {
      forAll(arbitrary[ArrivalStatusUpdate]) {
        arrivalStatusUpdate =>
          val expectedUpdateJson = Json.obj(
            "$set" -> Json.obj(
              "status"      -> arrivalStatusUpdate.arrivalStatus,
              "lastUpdated" -> LocalDateTime.now(clock).withSecond(0).withNano(0)
            )
          )

          ArrivalModifier.toJson(arrivalStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }

  "CompoundStatusUpdate" - {
    "ArrivalModifier returns modify object that would set the status and the message status" in {
      forAll(arbitrary[CompoundStatusUpdate]) {
        compoundStatusUpdate =>
          val expectedUpdateJson = Json.obj(
            "$set" -> Json.obj(
              "status"                                                                       -> compoundStatusUpdate.arrivalStatusUpdate.arrivalStatus,
              s"messages.${compoundStatusUpdate.messageStatusUpdate.messageId.index}.status" -> compoundStatusUpdate.messageStatusUpdate.messageStatus,
              "lastUpdated"                                                                  -> LocalDateTime.now(clock).withSecond(0).withNano(0)
            )
          )

          ArrivalModifier.toJson(compoundStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }

  "ArrivalPutUpdate" - {
    " ArrivalModifier returns modify object that would set the MRN, status and the message status" in {
      forAll(arbitrary[ArrivalPutUpdate]) {
        arrivalPutUpdate =>
          val expectedMessageId = arrivalPutUpdate.arrivalUpdate.messageStatusUpdate.messageId.index
          val expectedJson = Json.obj(
            "$set" -> Json.obj(
              "movementReferenceNumber"             -> arrivalPutUpdate.movementReferenceNumber,
              "status"                              -> arrivalPutUpdate.arrivalUpdate.arrivalStatusUpdate.arrivalStatus,
              s"messages.$expectedMessageId.status" -> arrivalPutUpdate.arrivalUpdate.messageStatusUpdate.messageStatus,
              "lastUpdated"                         -> LocalDateTime.now(clock).withSecond(0).withNano(0),
            )
          )

          ArrivalModifier.toJson(arrivalPutUpdate) mustEqual expectedJson
      }
    }
  }

}
