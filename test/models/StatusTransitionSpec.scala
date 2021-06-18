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

class StatusTransitionSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with EitherValues {
  "StatusTransition.transition" - {
    "Initialized must" - {

      "transition to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
        StatusTransition.transition(Initialized, MessageReceivedEvent.ArrivalSubmitted).value mustBe ArrivalSubmitted
      }

      "transition to Goods Released when receiving a GoodsReleased event" in {
        StatusTransition.transition(Initialized, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to Unloading Permsission when receiving a UnloadingPermission event" in {
        StatusTransition.transition(Initialized, MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "transition to Arrival Rejected when receiving a ArrivalRejected event" in {
        StatusTransition.transition(Initialized, MessageReceivedEvent.ArrivalRejected).value mustBe ArrivalRejected
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        StatusTransition.transition(Initialized, MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
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
            StatusTransition.transition(ArrivalStatus.Initialized, m).isLeft mustBe true
        }
      }
    }

    "Intitialized should not be transitioned to from any other state" ignore {
      val nonIntializedGen = arbitrary[ArrivalStatus].suchThat(_ != Initialized)

      forAll(nonIntializedGen, arbitrary[MessageReceivedEvent]) {
        case (status, event) =>
          StatusTransition.transition(status, event) must not equal (Initialized)

      }

    }

    "ArrivalSubmitted must " - {

      "do not transition to ArrivalSubmitted when receiving an ArrivalSubmitted event" in {
        StatusTransition.transition(ArrivalSubmitted, MessageReceivedEvent.ArrivalSubmitted).left.value mustBe TransitionError(
          "Can only accept this type of message [ArrivalSubmitted] directly after [ArrivalXMLSubmissionNegativeAcknowledgement or Initialized] messages. Current message state is [ArrivalSubmitted].")
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        StatusTransition.transition(ArrivalSubmitted, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to Unloading Permission when receiving a UnloadingPermission event" in {
        StatusTransition.transition(ArrivalSubmitted, MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "transition to ArrivalRejected when receiving a ArrivalRejected event" in {
        StatusTransition.transition(ArrivalSubmitted, MessageReceivedEvent.ArrivalRejected).value mustBe ArrivalRejected
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        StatusTransition.transition(ArrivalSubmitted, MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
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
            StatusTransition.transition(ArrivalStatus.ArrivalSubmitted, m).isLeft mustBe true
        }
      }
    }

    "UnloadingRemarksSubmitted must " - {

      "transition to Unloading Remarks Rejected when receiving an UnloadingRemarksRejected event" in {
        StatusTransition.transition(UnloadingRemarksSubmitted, MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
      }

      "transition to Unloading Remarks Rejected when receiving an UnloadingRemarksXMLSubmissionNegativeAcknowledgement event" in {
        StatusTransition
          .transition(UnloadingRemarksSubmitted, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
          .value mustBe UnloadingRemarksXMLSubmissionNegativeAcknowledgement
      }

      "do not transition to UnloadingRemarksSubmitted when receiving an UnloadingRemarksSubmitted event" in {
        StatusTransition.transition(UnloadingRemarksSubmitted, MessageReceivedEvent.UnloadingRemarksSubmitted).left.value mustBe TransitionError(
          "Can only accept this type of message [UnloadingRemarksSubmitted] directly after [UnloadingPermission or UnloadingRemarksRejected or UnloadingRemarksXMLSubmissionNegativeAcknowledgement] messages. Current message state is [UnloadingRemarksSubmitted].")
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        StatusTransition.transition(UnloadingRemarksSubmitted, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to UnloadingPermission state when receiving an UnloadingPermission event" in {
        StatusTransition.transition(UnloadingRemarksRejected, MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(
            MessageReceivedEvent.UnloadingRemarksRejected,
            MessageReceivedEvent.UnloadingRemarksSubmitted,
            MessageReceivedEvent.GoodsReleased,
            MessageReceivedEvent.UnloadingPermission,
            MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
          )
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.transition(ArrivalStatus.UnloadingRemarksSubmitted, m).isLeft mustBe true
        }
      }
    }

    "GoodsReleased must" - {

      "transition to GoodsReleased state when receiving a GoodsReleased event" in {
        StatusTransition.transition(GoodsReleased, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.transition(ArrivalStatus.GoodsReleased, m).isLeft mustBe true
        }
      }
    }

    "ArrivalRejected must " - {

      "transition to ArrivalRejected state when receiving an ArrivalRejected event" in {
        StatusTransition.transition(ArrivalRejected, MessageReceivedEvent.ArrivalRejected).value mustBe ArrivalRejected
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        StatusTransition.transition(ArrivalRejected, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.ArrivalRejected, MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.transition(ArrivalStatus.ArrivalRejected, m).isLeft mustBe true
        }
      }
    }

    "XMLSubmissionNegativeAcknowledgement must " - {

      "transition to XMLSubmissionNegativeAcknowledgement state when receiving an XMLSubmissionNegativeAcknowledgement event" in {
        StatusTransition.transition(ArrivalXMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value mustBe
          ArrivalXMLSubmissionNegativeAcknowledgement
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.ArrivalSubmitted)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.transition(ArrivalStatus.ArrivalXMLSubmissionNegativeAcknowledgement, m).isLeft mustBe true
        }
      }

      "IE917 for IE007" in {
        val step1 = StatusTransition.transition(Initialized, MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.transition(step1, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.transition(step2, MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

      }

      "IE917 for IE044" in {
        val step1 = StatusTransition.transition(Initialized, MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.transition(step1, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.transition(step2, MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

        val step4 = StatusTransition.transition(step3, MessageReceivedEvent.UnloadingPermission).value

        step4 mustEqual UnloadingPermission

        val step5 = StatusTransition.transition(step4, MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step5 mustEqual UnloadingRemarksSubmitted

        val step6 = StatusTransition.transition(step5, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step6 mustEqual UnloadingRemarksXMLSubmissionNegativeAcknowledgement

        val step7 = StatusTransition.transition(step6, MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step7 mustEqual UnloadingRemarksSubmitted

      }
    }

    "UnloadingRemarksXMLSubmissionNegativeAcknowledgement must " - {

      "transition to XMLSubmissionNegativeAcknowledgement state when receiving an XMLSubmissionNegativeAcknowledgement event" in {
        StatusTransition
          .transition(UnloadingRemarksXMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
          .value mustBe
          UnloadingRemarksXMLSubmissionNegativeAcknowledgement
      }

      "return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.UnloadingRemarksSubmitted)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.transition(ArrivalStatus.UnloadingRemarksXMLSubmissionNegativeAcknowledgement, m).isLeft mustBe true
        }
      }

      "IE917 for IE007" in {
        val step1 = StatusTransition.transition(Initialized, MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.transition(step1, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.transition(step2, MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

      }

      "IE917 for IE044" in {
        val step1 = StatusTransition.transition(Initialized, MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.transition(step1, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual ArrivalXMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.transition(step2, MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

        val step4 = StatusTransition.transition(step3, MessageReceivedEvent.UnloadingPermission).value

        step4 mustEqual UnloadingPermission

        val step5 = StatusTransition.transition(step4, MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step5 mustEqual UnloadingRemarksSubmitted

        val step6 = StatusTransition.transition(step5, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step6 mustEqual UnloadingRemarksXMLSubmissionNegativeAcknowledgement

        val step7 = StatusTransition.transition(step6, MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step7 mustEqual UnloadingRemarksSubmitted

      }
    }

    "UnloadingRemarksRejected must " - {

      "transition to UnloadingRemarksRejected state when receiving an UnloadingRemarksRejected event" in {
        StatusTransition.transition(UnloadingRemarksRejected, MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        StatusTransition.transition(UnloadingRemarksRejected, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        StatusTransition.transition(UnloadingRemarksRejected, MessageReceivedEvent.UnloadingRemarksSubmitted).value mustBe UnloadingRemarksSubmitted
      }

      "transition to UnloadingPermissionSubmitted state when receiving an UnloadingPermission event" in {
        StatusTransition.transition(UnloadingRemarksRejected, MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
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
            StatusTransition.transition(ArrivalStatus.UnloadingRemarksRejected, m).isLeft mustBe true
        }
      }
    }

    "UnloadingPermission must " - {

      "transition to UnloadingPermission state when receiving an UnloadingPermission event" in {
        StatusTransition.transition(UnloadingPermission, MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        StatusTransition.transition(UnloadingPermission, MessageReceivedEvent.UnloadingRemarksSubmitted).value mustBe UnloadingRemarksSubmitted
      }

      "transition to Goods Released state when receiving an GoodsReleased event" in {
        StatusTransition.transition(UnloadingPermission, MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.UnloadingPermission, MessageReceivedEvent.UnloadingRemarksSubmitted, MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.transition(ArrivalStatus.UnloadingPermission, m).isLeft mustBe true
        }
      }

    }

  }

}
