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
import models.MessageReceivedEvent._
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class StatusTransitionSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with EitherValues {
  "StatusTransition.transition" - {
    "Initialized must" - {

      "transition to ArrivalSubmitted when receiving a ArrivalSubmitted event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value mustBe ArrivalSubmitted
      }

      "transition to Goods Released when receiving a GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to Unloading Permsission when receiving a UnloadingPermission event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "transition to Arrival Rejected when receiving a ArrivalRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.ArrivalRejected).value mustBe ArrivalRejected
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
      }

      "not return an error message if any other event is provided" in {
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
            StatusTransition.targetStatus(m).isRight mustBe true
        }
      }
    }

    /**
      * ToDo CTCTRADERS-2634 - Remove?
    "Intitialized should not be transitioned to from any other state" ignore {
      val nonIntializedGen = arbitrary[ArrivalStatus].suchThat(_ != Initialized)

      forAll(nonIntializedGen, arbitrary[MessageReceivedEvent]) {
        case (_, event) =>
          StatusTransition.targetStatus(event) must not equal (Initialized)

      }

    }
      */
    "ArrivalSubmitted must " - {

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to Unloading Permission when receiving a UnloadingPermission event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "transition to ArrivalRejected when receiving a ArrivalRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.ArrivalRejected).value mustBe ArrivalRejected
      }

      "transition to Unloading remarks Rejected when receiving a UnloadingRemarksRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
      }

      "not return an error message if any other event is provided" in {
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
            StatusTransition.targetStatus(m).isRight mustBe true
        }
      }
    }

    "UnloadingRemarksSubmitted must " - {

      "transition to Unloading Remarks Rejected when receiving an UnloadingRemarksRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
      }

      "transition to Unloading Remarks Rejected when receiving an UnloadingRemarksXMLSubmissionNegativeAcknowledgement event" in {
        StatusTransition
          .targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
          .value mustBe XMLSubmissionNegativeAcknowledgement
      }

      "transition to GoodsReceived when receiving a GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to UnloadingPermission state when receiving an UnloadingPermission event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

    }

    "GoodsReleased must" - {

      "transition to GoodsReleased state when receiving a GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

    }

    "ArrivalRejected must " - {

      "transition to ArrivalRejected state when receiving an ArrivalRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.ArrivalRejected).value mustBe ArrivalRejected
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

    }

    "XMLSubmissionNegativeAcknowledgement must " - {

      "transition to XMLSubmissionNegativeAcknowledgement state when receiving an XMLSubmissionNegativeAcknowledgement event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value mustBe
          XMLSubmissionNegativeAcknowledgement
      }

      "IE917 for IE007" in {
        val step1 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual XMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

      }

      "IE917 for IE044" in {
        val step1 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual XMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

        val step4 = StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value

        step4 mustEqual UnloadingPermission

        val step5 = StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step5 mustEqual UnloadingRemarksSubmitted

        val step6 = StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step6 mustEqual XMLSubmissionNegativeAcknowledgement

        val step7 = StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step7 mustEqual UnloadingRemarksSubmitted

      }
    }

    "UnloadingRemarksXMLSubmissionNegativeAcknowledgement must " - {

      "transition to XMLSubmissionNegativeAcknowledgement state when receiving an XMLSubmissionNegativeAcknowledgement event" in {
        StatusTransition
          .targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
          .value mustBe
          XMLSubmissionNegativeAcknowledgement
      }

      "not return an error message if any other event is provided" in {
        val validMessages =
          Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.UnloadingRemarksSubmitted)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.targetStatus(m).isRight mustBe true
        }
      }

      "IE917 for IE007" in {
        val step1 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual XMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

      }

      "IE917 for IE044" in {
        val step1 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step1 mustEqual ArrivalSubmitted

        val step2 = StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step2 mustEqual XMLSubmissionNegativeAcknowledgement

        val step3 = StatusTransition.targetStatus(MessageReceivedEvent.ArrivalSubmitted).value

        step3 mustEqual ArrivalSubmitted

        val step4 = StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value

        step4 mustEqual UnloadingPermission

        val step5 = StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step5 mustEqual UnloadingRemarksSubmitted

        val step6 = StatusTransition.targetStatus(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement).value

        step6 mustEqual XMLSubmissionNegativeAcknowledgement

        val step7 = StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksSubmitted).value

        step7 mustEqual UnloadingRemarksSubmitted

      }
    }

    "UnloadingRemarksRejected must " - {

      "transition to UnloadingRemarksRejected state when receiving an UnloadingRemarksRejected event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksRejected).value mustBe UnloadingRemarksRejected
      }

      "transition to GoodsReleased state when receiving an GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksSubmitted).value mustBe UnloadingRemarksSubmitted
      }

      "transition to UnloadingPermissionSubmitted state when receiving an UnloadingPermission event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "not return an error message if any other event is provided" in {
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
            StatusTransition.targetStatus(m).isRight mustBe true
        }
      }
    }

    "UnloadingPermission must " - {

      "transition to UnloadingPermission state when receiving an UnloadingPermission event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingPermission).value mustBe UnloadingPermission
      }

      "transition to UnloadingRemarksSubmitted state when receiving an UnloadingRemarksSubmitted event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.UnloadingRemarksSubmitted).value mustBe UnloadingRemarksSubmitted
      }

      "transition to Goods Released state when receiving an GoodsReleased event" in {
        StatusTransition.targetStatus(MessageReceivedEvent.GoodsReleased).value mustBe GoodsReleased
      }

      "not return an error message if any other event is provided" in {
        val validMessages   = Seq(MessageReceivedEvent.UnloadingPermission, MessageReceivedEvent.UnloadingRemarksSubmitted, MessageReceivedEvent.GoodsReleased)
        val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
        invalidMessages.foreach {
          m =>
            StatusTransition.targetStatus(m).isRight mustBe true
        }
      }

    }

  }

}
