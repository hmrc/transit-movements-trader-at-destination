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
import models.ArrivalStatus._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrivalStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "Initialized must transition" - {

    "to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
      Initialized.transition(MessageReceived.ArrivalSubmitted) mustEqual ArrivalSubmitted
    }

    "to Goods Released when receiving a GoodsReleased event" in {
      Initialized.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }

    "to Arrival Rejected when receiving a Goods Rejection event" in {
      Initialized.transition(MessageReceived.ArrivalRejected) mustEqual ArrivalRejected
    }

  }

  "Intitialized should not be transitioned to from any other state" ignore {
    val nonIntializedGen = arbitrary[ArrivalStatus].suchThat(_ != Initialized)

    forAll(nonIntializedGen, arbitrary[MessageReceived]) {
      case (status, event) =>
        status.transition(event) must not equal (Initialized)

    }

  }

  "ArrivalSubmitted must transition" - {

    "to ArrivalSubmitted when receiving an ArrivalSubmitted event" in {
      ArrivalSubmitted.transition(MessageReceived.ArrivalSubmitted) mustEqual ArrivalSubmitted
    }

    "to GoodsReceived when receiving a GoodsReleased event" in {
      ArrivalSubmitted.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }

    "to ArrivalRejected when receiving a ArrivalRejected event" in {
      ArrivalSubmitted.transition(MessageReceived.ArrivalRejected) mustEqual ArrivalRejected
    }
  }

  "UnloadingRemarksSubmitted must transition" - {

    "to UnloadingRemarksSubmitted when receiving an UnloadingRemarksSubmitted event" in {
      UnloadingRemarksSubmitted.transition(MessageReceived.UnloadingRemarksSubmitted) mustEqual UnloadingRemarksSubmitted
    }

    "to GoodsReceived when receiving a GoodsReleased event" in {
      UnloadingRemarksSubmitted.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }
  }

  "GoodsReleased will always transition to GoodsReleased" in {
    forAll(arbitrary[MessageReceived]) {
      messageReceivedEvent =>
        GoodsReleased.transition(messageReceivedEvent) mustEqual GoodsReleased
    }
  }

  "ArrivalRejected must " - {

    "transform to ArrivalRejected state when receiving an ArrivalRejected event" in {
      ArrivalRejected.transition(MessageReceived.ArrivalRejected) mustEqual ArrivalRejected
    }

    "transform to GoodsReleased state when receiving an GoodsReleased event" in {
      ArrivalRejected.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }

    "throw exception when received an invalid message event" in {
      val x = intercept[Exception](ArrivalRejected.transition(MessageReceived.UnloadingPermission))
      x.getMessage mustEqual s"Tried to transition from ArrivalRejected to ${MessageReceived.UnloadingPermission}."
    }
  }

  "UnloadingPermission must transition" - {

    "to UnloadingPermission state when receiving an UnloadingPermission event" in {
      UnloadingPermission.transition(MessageReceived.UnloadingPermission) mustEqual UnloadingPermission
    }

    "to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
      UnloadingPermission.transition(MessageReceived.UnloadingRemarksSubmitted) mustEqual UnloadingRemarksSubmitted
    }
  }

}
