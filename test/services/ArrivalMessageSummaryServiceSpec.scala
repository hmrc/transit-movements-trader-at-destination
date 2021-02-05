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

package services

import base.SpecBase
import cats.data._
import generators.ModelGenerators
import models.MessageStatus._
import models.MessageType._
import models.Arrival
import models.MessageId
import models.MessageType
import models.MessagesSummary
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrivalMessageSummaryServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {
  import ArrivalMessageSummaryServiceSpec.MovementMessagesHelpers._

  def messageGeneratorSent(messageType: MessageType): Gen[MovementMessageWithStatus] = {
    val message = xml.XML.loadString(s"<${messageType.rootNode}>test</${messageType.rootNode}>")
    arbitrary[MovementMessageWithStatus].map(_.copy(messageType = messageType, message = message, status = SubmissionPending))
  }

  def messageGeneratorResponse(messageType: MessageType): Gen[MovementMessageWithoutStatus] = {
    val message = xml.XML.loadString(s"<${messageType.rootNode}>test</${messageType.rootNode}>")
    arbitrary[MovementMessageWithoutStatus].map(_.copy(messageType = messageType, message = message))
  }

  val ie007Gen = messageGeneratorSent(ArrivalNotification)
  val ie008Gen = messageGeneratorResponse(ArrivalRejection)
  val ie043Gen = messageGeneratorResponse(UnloadingPermission)
  val ie044Gen = messageGeneratorSent(UnloadingRemarks)
  val ie058Gen = messageGeneratorResponse(UnloadingRemarksRejection)
  val ie917Gen = messageGeneratorResponse(XMLSubmissionNegativeAcknowledgement)

  def arrivalMovement(msgs: NonEmptyList[MovementMessage]): Gen[Arrival] =
    for {
      arrival <- arbitrary[Arrival]
    } yield arrival.copy(messages = msgs)

  "arrivalNotificationR" - {

    "must return" - {

      "the original IE007 when there have been no other messages" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                val (message, messageId) = service.arrivalNotificationR(arrival)

                message mustEqual ie007
                messageId mustEqual MessageId.fromMessageIdValue(1).value

            }
        }
      }

      "the original IEOO7 and first IE008 when there is only an IE007 and a IE008" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie008Gen) {
          (ie007, ie008) =>
            val messages = NonEmptyList.of(ie007, ie008)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.arrivalNotificationR(arrival)

                message mustEqual ie007
                messageId mustEqual MessageId.fromMessageIdValue(1).value
            }
        }

      }

      "the new IEOO7 when there has been a correction to a rejected arrival message" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie008Gen.msgCorrId(1), ie007Gen.msgCorrId(2)) {
          case (ie007Old, ie008Old, ie007) =>
            val messages = NonEmptyList.of(ie007Old, ie008Old, ie007)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.arrivalNotificationR(arrival)

                message mustEqual ie007
                messageId mustEqual MessageId.fromMessageIdValue(3).value
            }
        }

      }

      "the latest IEOO7 when all IE007 have been rejected" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie008Gen.msgCorrId(1), ie007Gen.msgCorrId(2), ie008Gen.msgCorrId(2)) {
          case (ie007Old, ie008Old, ie007, ie008) =>
            val messages = NonEmptyList.of(ie007Old, ie008Old, ie007, ie008)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.arrivalNotificationR(arrival)

                message mustEqual ie007
                messageId mustEqual MessageId.fromMessageIdValue(3).value
            }
        }

      }
    }

  }

  "arrivalRejectionR" - {

    "must return" - {
      "None when there are none in the movement" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                service.arrivalRejectionR(arrival) must not be (defined)

            }
        }
      }

      "latest IE008 when there is only an IE007 and a IE008" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie008Gen) {
          (ie007, ie008) =>
            val messages = NonEmptyList.of(ie007, ie008)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.arrivalRejectionR(arrival).value

                message mustEqual ie008
                messageId mustEqual MessageId.fromMessageIdValue(2).value
            }
        }

      }

      "None when there has been an rejected arrival and correction arrival" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie008Gen.msgCorrId(1), ie007Gen.msgCorrId(2)) {
          case (ie007Old, ie008Old, ie007) =>
            val messages = NonEmptyList.of(ie007Old, ie008Old, ie007)

            forAll(arrivalMovement(messages)) {
              arrival =>
                service.arrivalRejectionR(arrival) must not be (defined)
            }
        }

      }

      "IE008 when all IE007 have been rejected" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie008Gen.msgCorrId(1), ie007Gen.msgCorrId(2), ie008Gen.msgCorrId(2)) {
          case (ie007Old, ie008Old, ie007, ie008) =>
            val messages = NonEmptyList.of(ie007Old, ie008Old, ie007, ie008)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.arrivalRejectionR(arrival).value

                message mustEqual ie008
                messageId mustEqual MessageId.fromMessageIdValue(4).value
            }
        }

      }
    }

  }

  "xmlSubmissionNegativeAcknowledgementR" - {

    "must return" - {
      "None when there are none in the movement" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                service.xmlSubmissionNegativeAcknowledgementR(arrival) must not be (defined)

            }
        }
      }

      "latest IE917 when there is only an IE007 and a IE917" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie917Gen) {
          (ie007, ie917) =>
            val messages = NonEmptyList.of(ie007, ie917)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.xmlSubmissionNegativeAcknowledgementR(arrival).value

                message mustEqual ie917
                messageId mustEqual MessageId.fromMessageIdValue(2).value
            }
        }

      }

      "IE917 when all IE007 have been rejected" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie917Gen.msgCorrId(1), ie007Gen.msgCorrId(2), ie917Gen.msgCorrId(2)) {
          case (ie007Old, ie917Old, ie007, ie917) =>
            val messages = NonEmptyList.of(ie007Old, ie917Old, ie007, ie917)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.xmlSubmissionNegativeAcknowledgementR(arrival).value

                message mustEqual ie917
                messageId mustEqual MessageId.fromMessageIdValue(4).value
            }
        }

      }
    }

  }

  "unloadingPermissionR" - {

    "must return" - {

      "None when IE043 does not exit" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                service.unloadingPermissionR(arrival) must not be (defined)

            }
        }
      }

      "None when IE007 does not exit" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie043Gen) {
          ie043 =>
            forAll(arrivalMovement(NonEmptyList.one(ie043))) {
              arrival =>
                service.unloadingPermissionR(arrival) must not be (defined)

            }
        }
      }

      "the latest IE043 when there is only an IE007 and a IE043" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie043Gen) {
          (ie007, ie043) =>
            val messages = NonEmptyList.of(ie007, ie043)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingPermissionR(arrival).value

                message mustEqual ie043
                messageId mustEqual MessageId.fromMessageIdValue(2).value
            }
        }

      }

      "latest IE043 when multiple IE043 messages exist" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie043Gen.msgCorrId(3)) {
          case (ie007, ie043Old, ie043) =>
            val messages = NonEmptyList.of(ie007, ie043Old, ie043)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingPermissionR(arrival).value

                message mustEqual ie043
                messageId mustEqual MessageId.fromMessageIdValue(3).value
            }
        }

      }

      "IE043 when IE044 exists" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie044Gen.msgCorrId(3)) {
          case (ie007, ie043, ie044) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingPermissionR(arrival).value

                message mustEqual ie043
                messageId mustEqual MessageId.fromMessageIdValue(2).value
            }
        }

      }

      "IE043 when IE044 and IE058 exists" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie044Gen.msgCorrId(3), ie058Gen.msgCorrId(4)) {
          case (ie007, ie043, ie044, ie058) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044, ie058)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingPermissionR(arrival).value

                message mustEqual ie043
                messageId mustEqual MessageId.fromMessageIdValue(2).value
            }
        }

      }

    }

  }

  "unloadingRemarksR" - {

    "must return" - {

      "None when IE044 does not exit" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                service.unloadingRemarksR(arrival) must not be (defined)

            }
        }
      }

      "IE044 if one exists" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie043Gen, ie044Gen) {
          (ie007, ie043, ie044) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingRemarksR(arrival).value

                message mustEqual ie044
            }
        }

      }

      "latest IE044 when IE044 messages exist" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie044Gen.msgCorrId(3), ie058Gen.msgCorrId(4), ie044Gen.msgCorrId(5)) {
          case (ie007, ie043, ie044Old, ie058, ie044) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044Old, ie058, ie044)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingRemarksR(arrival).value

                message mustEqual ie044
                messageId mustEqual MessageId.fromMessageIdValue(5).value
            }
        }

      }

      "IE044 when IE058 exists" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie044Gen.msgCorrId(3), ie058Gen.msgCorrId(4)) {
          case (ie007, ie043, ie044, ie058) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044, ie058)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingRemarksR(arrival).value

                message mustEqual ie044
                messageId mustEqual MessageId.fromMessageIdValue(3).value
            }
        }

      }

      "latest IE044 when multiple IE058 messages exist" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1),
               ie043Gen.msgCorrId(2),
               ie044Gen.msgCorrId(3),
               ie058Gen.msgCorrId(4),
               ie044Gen.msgCorrId(5),
               ie058Gen.msgCorrId(6)) {
          case (ie007, ie043, ie044Old, ie058Old, ie044, ie058) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044Old, ie058Old, ie044, ie058)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingRemarksR(arrival).value

                message mustEqual ie044
                messageId mustEqual MessageId.fromMessageIdValue(5).value
            }
        }

      }

    }

  }

  "unloadingRemarksRejectionsR" - {

    "must return" - {

      "None when IE058 does not exit" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                service.unloadingRemarksRejectionsR(arrival) must not be (defined)

            }
        }
      }

      "IE058 when only one exists" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie043Gen, ie044Gen, ie058Gen) {
          (ie007, ie043, ie044, ie058) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044, ie058)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingRemarksRejectionsR(arrival).value

                message mustEqual ie058
                messageId mustEqual MessageId.fromMessageIdValue(4).value
            }
        }

      }

      "latest IE058 when multiple IE058 messages exist" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1),
               ie043Gen.msgCorrId(2),
               ie044Gen.msgCorrId(3),
               ie058Gen.msgCorrId(4),
               ie044Gen.msgCorrId(5),
               ie058Gen.msgCorrId(6)) {
          case (ie007, ie043, ie044Old, ie058Old, ie044, ie058) =>
            val messages = NonEmptyList.of(ie007, ie043, ie044Old, ie058Old, ie044, ie058)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val (message, messageId) = service.unloadingRemarksRejectionsR(arrival).value

                message mustEqual ie058
                messageId mustEqual MessageId.fromMessageIdValue(6).value
            }
        }

      }
    }

  }

  "arrivalMessagesSummary" - {

    "must return" - {

      "initial IE007 and no IE008 when there is only a IE007" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen) {
          ie007 =>
            forAll(arrivalMovement(NonEmptyList.one(ie007))) {
              arrival =>
                service.arrivalMessagesSummary(arrival) mustEqual MessagesSummary(arrival, MessageId.fromMessageIdValue(1).value, None)

            }
        }
      }

      "latest IE008 when there is only an IE007 and a IE008" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen, ie008Gen) {
          (ie007, ie008) =>
            val messages = NonEmptyList.of(ie007, ie008)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary = MessagesSummary(arrival, MessageId.fromMessageIdValue(1).value, MessageId.fromMessageIdValue(2))

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

      "latest IE008 when there has been an IE007 correction" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie008Gen.msgCorrId(2), ie007Gen.msgCorrId(3)) {
          case (ie007Old, ie008Old, ie007) =>
            val messages = NonEmptyList.of(ie007Old, ie008Old, ie007)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary = MessagesSummary(arrival, MessageId.fromMessageIdValue(3).value, None)

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

      "IE008 when all IE007 have been rejected" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie008Gen.msgCorrId(2), ie007Gen.msgCorrId(3), ie008Gen.msgCorrId(4)) {
          case (ie007Old, ie008Old, ie007, ie008) =>
            val messages = NonEmptyList.of(ie007Old, ie008Old, ie007, ie008)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary = MessagesSummary(arrival, MessageId.fromMessageIdValue(3).value, MessageId.fromMessageIdValue(4))

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

      "IE007 and IE043" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2)) {
          case (ie007, ie0043) =>
            val messages = NonEmptyList.of(ie007, ie0043)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary =
                  MessagesSummary(arrival, MessageId.fromMessageIdValue(1).value, None, MessageId.fromMessageIdValue(2), None, None)

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

      "IE007, IE043 and IE044" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie044Gen.msgCorrId(3)) {
          case (ie007, ie0043, ie044) =>
            val messages = NonEmptyList.of(ie007, ie0043, ie044)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary =
                  MessagesSummary(arrival, MessageId.fromMessageIdValue(1).value, None, MessageId.fromMessageIdValue(2), MessageId.fromMessageIdValue(3), None)

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

      "IE044 and no IE058 when there has been an IE044 correction" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1), ie043Gen.msgCorrId(2), ie044Gen.msgCorrId(3), ie058Gen.msgCorrId(4), ie044Gen.msgCorrId(5)) {
          case (ie007, ie0043, ie044Old, ie058, ie044) =>
            val messages = NonEmptyList.of(ie007, ie0043, ie044Old, ie058, ie044)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary =
                  MessagesSummary(arrival, MessageId.fromMessageIdValue(1).value, None, MessageId.fromMessageIdValue(2), MessageId.fromMessageIdValue(5), None)

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

      "latest IE044 and IE058 when there has been multiple corrections without a successful IE044 correction" in {
        val service = new ArrivalMessageSummaryService

        forAll(ie007Gen.submitted.msgCorrId(1),
               ie043Gen.msgCorrId(2),
               ie044Gen.msgCorrId(3),
               ie058Gen.msgCorrId(4),
               ie044Gen.msgCorrId(5),
               ie058Gen.msgCorrId(6)) {
          case (ie007, ie0043, ie044Old, ie058Old, ie044, ie058) =>
            val messages = NonEmptyList.of(ie007, ie0043, ie044Old, ie058Old, ie044, ie058)

            forAll(arrivalMovement(messages)) {
              arrival =>
                val expectedMessageSummary =
                  MessagesSummary(arrival,
                                  MessageId.fromMessageIdValue(1).value,
                                  None,
                                  MessageId.fromMessageIdValue(2),
                                  MessageId.fromMessageIdValue(5),
                                  MessageId.fromMessageIdValue(6))

                service.arrivalMessagesSummary(arrival) mustEqual expectedMessageSummary
            }
        }

      }

    }

  }
}

object ArrivalMessageSummaryServiceSpec {

  object MovementMessagesHelpers {

    implicit class SubmittedOps(movementMessageWithStatus: Gen[MovementMessageWithStatus]) {
      def submitted: Gen[MovementMessageWithStatus] = movementMessageWithStatus.map(_.copy(status = SubmissionSucceeded))
    }

    implicit class MessageCorrelationIdOps(movementMessageWithStatus: Gen[MovementMessageWithStatus]) {
      def msgCorrId(value: Int): Gen[MovementMessage] = movementMessageWithStatus.map(_.copy(messageCorrelationId = value))
    }

    implicit class MessageCorrelationIdOps2(movementMessageWithStatus: Gen[MovementMessageWithoutStatus]) {
      def msgCorrId(value: Int): Gen[MovementMessage] = movementMessageWithStatus.map(_.copy(messageCorrelationId = value))
    }

  }

}
