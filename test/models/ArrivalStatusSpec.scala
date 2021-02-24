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
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrivalStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with EitherValues {
  "transition" - {
    "Initialized must" - {

      "transition to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
        Initialized.transition(MessageReceivedEvent.ArrivalSubmitted).right.value mustBe ArrivalSubmitted
      }

      "transition to Goods Released when receiving a GoodsReleased event" in {
        Initialized.transition(MessageReceivedEvent.GoodsReleased).right.value mustBe GoodsReleased
      }

      "transition to Unloading Permsission when receiving a UnloadingPermission event" in {
        Initialized.transition(MessageReceivedEvent.UnloadingPermission).right.value mustBe UnloadingPermission
      }

      "transition to Arrival Rejected when receiving a ArrivalRejected event" in {
        Initialized.transition(MessageReceivedEvent.ArrivalRejected).right.value mustBe ArrivalRejected
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        Initialized.transition(MessageReceivedEvent.UnloadingRemarksRejected).right.value mustBe UnloadingRemarksRejected
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
        ArrivalSubmitted.transition(MessageReceivedEvent.ArrivalSubmitted).right.value mustBe ArrivalSubmitted
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.GoodsReleased).right.value mustBe GoodsReleased
      }

      "transition to Unloading Permission when receiving a UnloadingPermission event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingPermission).right.value mustBe UnloadingPermission
      }

      "transition to ArrivalRejected when receiving a ArrivalRejected event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.ArrivalRejected).right.value mustBe ArrivalRejected
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        ArrivalSubmitted.transition(MessageReceivedEvent.UnloadingRemarksRejected).right.value mustBe UnloadingRemarksRejected
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
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.UnloadingRemarksRejected).right.value mustBe UnloadingRemarksRejected
      }

      "transition to Unloading Remarks Rejected when receiving an UnloadingRemarksXMLSubmissionNegativeAcknowledgement event" in {
        UnloadingRemarksSubmitted
          .transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
          .right
          .value mustBe UnloadingRemarksXMLSubmissionNegativeAcknowledgement
      }

      "transition to UnloadingRemarksSubmitted when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value mustBe UnloadingRemarksSubmitted
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        UnloadingRemarksSubmitted.transition(MessageReceivedEvent.GoodsReleased).right.value mustBe GoodsReleased
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(
            MessageReceivedEvent.UnloadingRemarksRejected,
            MessageReceivedEvent.UnloadingRemarksSubmitted,
            MessageReceivedEvent.GoodsReleased,
            MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
          )
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
          GoodsReleased.transition(messageReceivedEvent).right.value mustBe GoodsReleased
      }
    }

    "ArrivalRejected must " - {

      "transition to ArrivalRejected state when receiving an ArrivalRejected event" in {
        ArrivalRejected.transition(MessageReceivedEvent.ArrivalRejected).right.value mustBe ArrivalRejected
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        ArrivalRejected.transition(MessageReceivedEvent.GoodsReleased).right.value mustBe GoodsReleased
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
        ArrivalXMLSubmissionNegativeAcknowledgement.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value mustBe
          ArrivalXMLSubmissionNegativeAcknowledgement
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.ArrivalSubmitted)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.ArrivalXMLSubmissionNegativeAcknowledgement.transition(m).isLeft mustBe true
        }
      }

      "IE917 for IE007" in {
        val step1 = Initialized.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step1 mustEqual ArrivalSubmitted

        val step2 = step1.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = step2.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step3 mustEqual ArrivalSubmitted

      }

      "IE917 for IE044" in {
        val step1 = Initialized.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step1 mustEqual ArrivalSubmitted

        val step2 = step1.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = step2.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step3 mustEqual ArrivalSubmitted

        val step4 = step3.transition(MessageReceivedEvent.UnloadingPermission).right.value

        step4 mustEqual UnloadingPermission

        val step5 = step4.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value

        step5 mustEqual UnloadingRemarksSubmitted

        val step6 = step5.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value

        step6 mustEqual UnloadingRemarksXMLSubmissionNegativeAcknowledgement

        val step7 = step6.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value

        step7 mustEqual UnloadingRemarksSubmitted

      }
    }

    "UnloadingRemarksXMLSubmissionNegativeAcknowledgement must " - {

      "transition to XMLSubmissionNegativeAcknowledgement state when receiving an XMLSubmissionNegativeAcknowledgement event" in {
        UnloadingRemarksXMLSubmissionNegativeAcknowledgement.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value mustBe
          UnloadingRemarksXMLSubmissionNegativeAcknowledgement
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.UnloadingRemarksSubmitted)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.UnloadingRemarksXMLSubmissionNegativeAcknowledgement.transition(m).isLeft mustBe true
        }
      }

      "IE917 for IE007" in {
        val step1 = Initialized.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step1 mustEqual ArrivalSubmitted

        val step2 = step1.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = step2.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step3 mustEqual ArrivalSubmitted

      }

      "IE917 for IE044" in {
        val step1 = Initialized.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step1 mustEqual ArrivalSubmitted

        val step2 = step1.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = step2.transition(MessageReceivedEvent.ArrivalSubmitted).right.value

        step3 mustEqual ArrivalSubmitted

        val step4 = step3.transition(MessageReceivedEvent.UnloadingPermission).right.value

        step4 mustEqual UnloadingPermission

        val step5 = step4.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value

        step5 mustEqual UnloadingRemarksSubmitted

        val step6 = step5.transition(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).right.value

        step6 mustEqual UnloadingRemarksXMLSubmissionNegativeAcknowledgement

        val step7 = step6.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value

        step7 mustEqual UnloadingRemarksSubmitted

      }
    }

    "UnloadingRemarksRejected must " - {

      "transition to UnloadingRemarksRejected state when receiving an UnloadingRemarksRejected event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingRemarksRejected).right.value mustBe UnloadingRemarksRejected
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.GoodsReleased).right.value mustBe GoodsReleased
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value mustBe UnloadingRemarksSubmitted
      }

      "transition to UnloadingPermissionSubmitted state when receiving an UnloadingPermission event" in {
        UnloadingRemarksRejected.transition(MessageReceivedEvent.UnloadingPermission).right.value mustBe UnloadingPermission
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(
            MessageReceivedEvent.UnloadingRemarksRejected,
            MessageReceivedEvent.GoodsReleased,
            MessageReceivedEvent.UnloadingRemarksSubmitted,
            MessageReceivedEvent.UnloadingPermission
          )
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            ArrivalStatus.UnloadingRemarksRejected.transition(m).isLeft mustBe true
        }
      }
    }

    "UnloadingPermission must " - {

      "transition to UnloadingPermission state when receiving an UnloadingPermission event" in {
        UnloadingPermission.transition(MessageReceivedEvent.UnloadingPermission).right.value mustBe UnloadingPermission
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        UnloadingPermission.transition(MessageReceivedEvent.UnloadingRemarksSubmitted).right.value mustBe UnloadingRemarksSubmitted
      }

      "transition to Goods Released state when receiving an GoodsReleased event" in {
        UnloadingPermission.transition(MessageReceivedEvent.GoodsReleased).right.value mustBe GoodsReleased
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
