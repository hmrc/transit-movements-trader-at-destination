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

package models.state

import base.SpecBase
import generators.ModelGenerators
import models.ArrivalState._
import models.ArrivalState
import models.MessageReceived
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrivalStateSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "Initialized must transition" - {

    "to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
      Initialized.transition(MessageReceived.ArrivalSubmitted) mustEqual ArrivalSubmitted
    }

    "to Goods Released when receiving a GoodsReleased event" in {
      Initialized.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }

  }

  "Intitialized should not be transitioned to from any other state" in {
    val nonIntializedGen = arbitrary[ArrivalState].suchThat(_ != Initialized)

    forAll(nonIntializedGen, arbitrary[MessageReceived]) {
      case (state, event) =>
        state.transition(event) must not equal (Initialized)

    }

  }

  "ArrivalSubmitted must transition" - {

    "to ArrivalSubmitted when receiving an ArrivalSubmitted event" in {
      ArrivalSubmitted.transition(MessageReceived.ArrivalSubmitted) mustEqual ArrivalSubmitted
    }

    "to GoodsReceived when receiving a GoodsReleased event" in {
      ArrivalSubmitted.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }
  }

  "GoodsReleased will always transition to GoodsReleased" in {
    forAll(arbitrary[MessageReceived]) {
      messageReceivedEvent =>
        GoodsReleased.transition(messageReceivedEvent) mustEqual GoodsReleased
    }
  }

}
