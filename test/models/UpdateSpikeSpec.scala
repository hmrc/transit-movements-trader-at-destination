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

import cats._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json

class UpdateSpikeSpec extends AnyFreeSpec with Matchers {

  val aStatusUpdate  = AStatusUpdate(ArrivalStatus.Initialized)
  val aStatusUpdate2 = AStatusUpdate(ArrivalStatus.ArrivalSubmitted)

  val mStatusUpdate  = MStatusUpdate(MessageId.fromIndex(1), MessageStatus.SubmissionPending)
  val mStatusUpdate2 = MStatusUpdate(MessageId.fromIndex(2), MessageStatus.SubmissionPending)
  val mStatusUpdate3 = MStatusUpdate(MessageId.fromIndex(1), MessageStatus.SubmissionSucceeded)
  val mStatusUpdate4 = MStatusUpdate(MessageId.fromIndex(2), MessageStatus.SubmissionSucceeded)

  "aStatusUpdate" - {
    "aStatusUpdate + aStatusUpdate = rhs" in {
      Semigroup.combine[UpdateSpike](aStatusUpdate, aStatusUpdate2) mustEqual aStatusUpdate2
    }
  }

  "ArrivalModifier" - {
    "asdf" in {

      val expectedJson = Json.obj(
        "$set" -> Json.obj(
          "status" -> aStatusUpdate.arrivalStatus
        )
      )

      ArrivalModifier.toJson[UpdateSpike](aStatusUpdate) mustEqual expectedJson

    }
  }

}
