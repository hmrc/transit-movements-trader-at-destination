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

import base.SpecBase
import generators.ModelGenerators
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

class ArrivalPutUpdateSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  implicit val arbitraryArrivalPutUpdate = Arbitrary {
    for {
      mrn           <- arbitrary[MovementReferenceNumber]
      arrivalUpdate <- generatorArrivalUpdate
    } yield ArrivalPutUpdate(mrn, arrivalUpdate)
  }

  "ArrivalModifier for ArrivalPutUpdate" - {
    "should return the modify object that would set the MRN, status and the message status" in {
      forAll(arbitrary[ArrivalPutUpdate]) {
        arrivalPutUpdate =>
          val expectedMessageId = arrivalPutUpdate.arrivalUpdate.messageUpdate.value.messageId.index
          val expectedJson = Json.obj(
            "$set" -> Json.obj(
              "movementReferenceNumber"             -> arrivalPutUpdate.movementReferenceNumber,
              "status"                              -> arrivalPutUpdate.arrivalUpdate.arrivalUpdate.value,
              s"messages.$expectedMessageId.status" -> arrivalPutUpdate.arrivalUpdate.messageUpdate.value.messageStatus
            )
          )

          ArrivalModifier.toJson(arrivalPutUpdate) mustEqual expectedJson
      }
    }
  }

}
