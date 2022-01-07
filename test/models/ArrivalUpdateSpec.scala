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

import base.FreeSpecDiscipline
import base.SpecBase
import cats._
import cats.kernel.laws.discipline.SemigroupTests
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

import java.time.Clock
import java.time.LocalDateTime
import java.time.ZoneOffset

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

  "ArrivalPutUpdate" - {
    " ArrivalModifier returns modify object that would set the MRN, status and the message status" in {
      forAll(arbitrary[ArrivalPutUpdate]) {
        arrivalPutUpdate =>
          val expectedMessageId = arrivalPutUpdate.messageStatusUpdate.messageId.index
          val expectedJson = Json.obj(
            "$set" -> Json.obj(
              "movementReferenceNumber"             -> arrivalPutUpdate.movementReferenceNumber,
              s"messages.$expectedMessageId.status" -> arrivalPutUpdate.messageStatusUpdate.messageStatus,
              "lastUpdated"                         -> LocalDateTime.now(clock).withSecond(0).withNano(0)
            )
          )

          ArrivalModifier.toJson(arrivalPutUpdate) mustEqual expectedJson
      }
    }
  }

}
