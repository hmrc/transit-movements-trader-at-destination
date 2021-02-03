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
import models.ArrivalStatus._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrivalStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {
  "transition" - {
    "Initialized must" - {

      "transition to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
        Initialized.transition(MessageReceivedEvent.ArrivalSubmitted) mustBe Right(ArrivalSubmitted)
      }

      "transition to Goods Released when receiving a GoodsReleased event" in {
        Initialized.transition(MessageReceivedEvent.GoodsReleased) mustBe Right(GoodsReleased)
      }

      "transition to Unloading Permsission when receiving a UnloadingPermission event" in {
        Initialized.transition(MessageReceivedEvent.UnloadingPermission) mustBe Right(UnloadingPermission)
      }

      "transition to Arrival Rejected when receiving a ArrivalRejected event" in {
        Initialized.transition(MessageReceivedEvent.ArrivalRejected) mustBe Right(ArrivalRejected)
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        Initialized.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustBe Right(UnloadingRemarksRejected)
      }

      "return an error message if any other event is provided" in {
        val validMessages = Seq(
          MessageReceivedEvent.ArrivalSubmitted,
          MessageReceivedEvent.GoodsReleased,
          MessageReceivedEvent.UnloadingPermission,
          MessageReceivedEvent.ArrivalRejected,
          MessageReceivedEvent.UnloadingRemarksRejected,
          MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
        )
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.Initialized.transition(m).isLeft mustBe true
        }
      }
    }

    "Intitialized should not be transitioned to from any other state" ignore {
      val nonIntializedGen = arbitrary[ArrivalStatus].suchThat(_ != Initialized)

      forAll(nonIntializedGen, arbitrary[MessageReceivedEvent]) {
        case (status, event) =>
          status.transition(event) must not equal (Initialized)

      }

    }

    "ArrivalSubmitted must " - {

      "transition to ArrivalSubmitted when receiving an ArrivalSubmitted event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.ArrivalSubmitted) mustBe Right(ArrivalSubmitted)
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.GoodsReleased) mustBe Right(GoodsReleased)
      }

      "transition to Unloading Permission when receiving a UnloadingPermission event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingPermission) mustBe Right(UnloadingPermission)
      }

      "transition to ArrivalRejected when receiving a ArrivalRejected event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.ArrivalRejected) mustBe Right(ArrivalRejected)
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustBe Right(UnloadingRemarksRejected)
      }

      "return an error message if any other event is provided" in {
        val validMessages = Seq(
          MessageReceivedEvent.ArrivalSubmitted,
          MessageReceivedEvent.GoodsReleased,
          MessageReceivedEvent.UnloadingPermission,
          MessageReceivedEvent.ArrivalRejected,
          MessageReceivedEvent.UnloadingRemarksRejected,
          MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
        )
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.ArrivalSubmitted.transition(m).isLeft mustBe true
        }
      }
    }

    "UnloadingRemarksSubmitted must " - {

      "transition to Unloading Remarks Rejected when receiving an UnloadingRemarksRejected event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustBe Right(UnloadingRemarksRejected)
      }

      "transition to UnloadingRemarksSubmitted when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.UnloadingRemarksSubmitted) mustBe Right(UnloadingRemarksSubmitted)
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.GoodsReleased) mustBe Right(GoodsReleased)
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.UnloadingRemarksRejected, MessageReceivedEvent.UnloadingRemarksSubmitted, MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.UnloadingRemarksSubmitted.transition(m).isLeft mustBe true
        }
      }
    }

    "GoodsReleased will always transition to GoodsReleased" in {
      forAll(arbitrary[MessageReceivedEvent]) {
        messageReceivedEvent =>
          GoodsReleased.transition(messageReceivedEvent) mustBe Right(GoodsReleased)
      }
    }

    "ArrivalRejected must " - {

      "transition to ArrivalRejected state when receiving an ArrivalRejected event" in {
        ArrivalRejected.transition(MessageReceivedEvent.ArrivalRejected) mustBe Right(ArrivalRejected)
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        ArrivalRejected.transition(MessageReceivedEvent.GoodsReleased) mustBe Right(GoodsReleased)
      }

      "return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.ArrivalRejected, MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.ArrivalRejected.transition(m).isLeft mustBe true
        }
      }
    }

    "XMLSubmissionNegativeAcknowledgement must " - {

      "transition to XMLSubmissionNegativeAcknowledgement state when receiving an XMLSubmissionNegativeAcknowledgement event" in {
        XMLSubmissionNegativeAcknowledgement.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement) mustBe Right(
          XMLSubmissionNegativeAcknowledgement)
      }

      "return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.XMLSubmissionNegativeAcknowledgement.transition(m).isLeft mustBe true
        }
      }
    }

    "UnloadingRemarksRejected must " - {

      "transition to UnloadingRemarksRejected state when receiving an UnloadingRemarksRejected event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingRemarksRejected) mustBe Right(UnloadingRemarksRejected)
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.GoodsReleased) mustBe Right(GoodsReleased)
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingRemarksSubmitted) mustBe Right(UnloadingRemarksSubmitted)
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.UnloadingRemarksRejected, MessageReceivedEvent.GoodsReleased, MessageReceivedEvent.UnloadingRemarksSubmitted)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.UnloadingRemarksRejected.transition(m).isLeft mustBe true
        }
      }
    }

    "UnloadingPermission must " - {

      "transition to UnloadingPermission state when receiving an UnloadingPermission event" in {
        UnloadingPermission.transition(MessageReceivedEvent.UnloadingPermission) mustBe Right(UnloadingPermission)
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingPermission.transition(MessageReceivedEvent.UnloadingRemarksSubmitted) mustBe Right(UnloadingRemarksSubmitted)
      }

      "transition to Goods Released state when receiving an GoodsReleased event" in {
        UnloadingPermission.transition(MessageReceivedEvent.GoodsReleased) mustBe Right(GoodsReleased)
      }

      "return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.UnloadingPermission, MessageReceivedEvent.UnloadingRemarksSubmitted, MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.UnloadingPermission.transition(m).isLeft mustBe true
        }
      }

    }
  }

}
