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
  "transition" - {
    "Initialized must transition" - {

      "to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
        Initialized.transition(MessageReceivedEvent.ArrivalSubmitted) mustEqual ArrivalSubmitted
      }

      "to Goods Released when receiving a GoodsReleased event" in {
        Initialized.transition(MessageReceivedEvent.GoodsReleased) mustEqual GoodsReleased
      }

      "to Unloading Permsission when receiving a UnloadingPermission event" in {
        Initialized.transition(MessageReceivedEvent.UnloadingPermission) mustEqual UnloadingPermission
      }

      "to Arrival Rejected when receiving aArrivalRejected event" in {
        Initialized.transition(MessageReceivedEvent.ArrivalRejected) mustEqual ArrivalRejected
      }

      "to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        Initialized.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustEqual UnloadingRemarksRejected
      }
    }

    "Intitialized should not be transitioned to from any other state" ignore {
      val nonIntializedGen = arbitrary[ArrivalStatus].suchThat(_ != Initialized)

      forAll(nonIntializedGen, arbitrary[MessageReceivedEvent]) {
        case (status, event) =>
          status.transition(event) must not equal (Initialized)

      }

    }

    "ArrivalSubmitted must transition" - {

      "to ArrivalSubmitted when receiving an ArrivalSubmitted event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.ArrivalSubmitted) mustEqual ArrivalSubmitted
      }

      "to GoodsReceived when receiving a GoodsReleased event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.GoodsReleased) mustEqual GoodsReleased
      }

      "to Unloading Permission when receiving a UnloadingPermission event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingPermission) mustEqual UnloadingPermission
      }

      "to ArrivalRejected when receiving a ArrivalRejected event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.ArrivalRejected) mustEqual ArrivalRejected
      }

      "to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustEqual UnloadingRemarksRejected
      }

      "throw exception when received an invalid message event" in {
        val exception = intercept[Exception](ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingRemarksSubmitted))
        exception.getMessage mustEqual s"Tried to transition from ArrivalSubmitted to ${MessageReceivedEvent.UnloadingRemarksSubmitted}."
      }
    }

    "UnloadingRemarksSubmitted must transition" - {

      "to Unloading Remarks Rejected when receiving an UnloadingRemarksRejected event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustEqual UnloadingRemarksRejected
      }

      "to UnloadingRemarksSubmitted when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.UnloadingRemarksSubmitted) mustEqual UnloadingRemarksSubmitted
      }

      "to GoodsReceived when receiving a GoodsReleased event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.GoodsReleased) mustEqual GoodsReleased
      }

      "throw exception when received an invalid message event" in {
        val exception = intercept[Exception](UnloadingRemarksSubmitted.transition(MessageReceivedEvent.ArrivalSubmitted))
        exception.getMessage mustEqual s"Tried to transition from UnloadingRemarksSubmitted to ${MessageReceivedEvent.ArrivalSubmitted}."
      }
    }

    "GoodsReleased will always transition to GoodsReleased" in {
      forAll(arbitrary[MessageReceivedEvent]) {
        messageReceivedEvent =>
          GoodsReleased.transition(messageReceivedEvent) mustEqual GoodsReleased
      }
    }

    "ArrivalRejected must " - {

      "transform to ArrivalRejected state when receiving an ArrivalRejected event" in {
        ArrivalRejected.transition(MessageReceivedEvent.ArrivalRejected) mustEqual ArrivalRejected
      }

      "transform to GoodsReleased state when receiving an GoodsReleased event" in {
        ArrivalRejected.transition(MessageReceivedEvent.GoodsReleased) mustEqual GoodsReleased
      }

      "throw exception when received an invalid message event" in {
        val exception = intercept[Exception](ArrivalRejected.transition(MessageReceivedEvent.UnloadingPermission))
        exception.getMessage mustEqual s"Tried to transition from ArrivalRejected to ${MessageReceivedEvent.UnloadingPermission}."
      }
    }

    "UnloadingRemarksRejected must " - {

      "transform to UnloadingRemarksRejected state when receiving an UnloadingRemarksRejected event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustEqual UnloadingRemarksRejected
      }

      "transform to GoodsReleased state when receiving an GoodsReleased event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.GoodsReleased) mustEqual GoodsReleased
      }

      "transform to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingRemarksSubmitted) mustEqual UnloadingRemarksSubmitted
      }

      "throw exception when received an invalid message event" in {
        val exception = intercept[Exception](UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingPermission))
        exception.getMessage mustEqual s"Tried to transition from UnloadingRemarksRejected to ${MessageReceivedEvent.UnloadingPermission}."
      }
    }

    "UnloadingPermission must transition" - {

      "to UnloadingPermission state when receiving an UnloadingPermission event" in {
        UnloadingPermission.transition(MessageReceivedEvent.UnloadingPermission) mustEqual UnloadingPermission
      }

      "to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingPermission.transition(MessageReceivedEvent.UnloadingRemarksSubmitted) mustEqual UnloadingRemarksSubmitted
      }

      "to Goods Released state when receiving an GoodsReleased event" in {
        UnloadingPermission.transition(MessageReceivedEvent.GoodsReleased) mustEqual GoodsReleased
      }

      "throw exception when received an invalid message event" in {
        val exception = intercept[Exception](UnloadingPermission.transition(MessageReceivedEvent.ArrivalRejected))
        exception.getMessage mustEqual s"Tried to transition from ArrivalSubmitted to ${MessageReceivedEvent.ArrivalRejected}."
      }

    }
  }

}
