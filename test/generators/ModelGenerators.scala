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

package generators

import java.time._

import models.MessageStatus.SubmissionPending
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.ArrivalUpdate
import models.MessageId
import models.MessageReceivedEvent
import models.MessageStatus
import models.MessageStatusUpdate
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import models.RejectionError
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait ModelGenerators extends BaseGenerators with JavaTimeGenerators {

  private val pastDate: LocalDate = LocalDate.of(1900, 1, 1)
  private val dateNow: LocalDate  = LocalDate.now

  implicit val arbitraryMessageStatusUpdate: Arbitrary[MessageStatusUpdate] =
    Arbitrary {
      for {
        messageId     <- arbitrary[MessageId]
        messageStatus <- arbitrary[MessageStatus]
      } yield MessageStatusUpdate(messageId, messageStatus)
    }

  implicit val arbitraryArrivalUpdate: Arbitrary[ArrivalUpdate] =
    Arbitrary {
      for {
        arrivalStatus       <- arbitrary[Option[ArrivalStatus]]
        messageStatusUpdate <- arbitrary[Option[MessageStatusUpdate]]
      } yield ArrivalUpdate(arrivalStatus, messageStatusUpdate)
    }

  implicit val generatorArrivalUpdate: Gen[ArrivalUpdate] =
    for {
      arrivalStatus <- arbitrary[ArrivalStatus]
      messageStatus <- arbitrary[MessageStatusUpdate]
    } yield ArrivalUpdate(Some(arrivalStatus), Some(messageStatus))

  implicit lazy val arbitraryMessageStatus: Arbitrary[MessageStatus] =
    Arbitrary {
      Gen.oneOf(MessageStatus.values)
    }

  implicit lazy val arbitraryMessageWithStateXml: Arbitrary[MovementMessageWithStatus] = {
    Arbitrary {
      for {
        dateTime    <- arbitrary[LocalDateTime]
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
        status = SubmissionPending
      } yield MovementMessageWithStatus(dateTime, messageType, xml, status, 1)
    }
  }

  implicit lazy val arbitraryMessageWithoutStateXml: Arbitrary[MovementMessageWithoutStatus] = {
    Arbitrary {
      for {
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
      } yield MovementMessageWithoutStatus(LocalDateTime.of(date, time), messageType, xml, 1)
    }
  }

  implicit lazy val arbitraryArrivalId: Arbitrary[ArrivalId] = {
    Arbitrary {
      for {
        id <- intWithMaxLength(9)
      } yield ArrivalId(id)
    }
  }

  implicit lazy val arbitraryState: Arbitrary[ArrivalStatus] =
    Arbitrary {
      Gen.oneOf(ArrivalStatus.values)
    }

  implicit lazy val arbitraryArrival: Arbitrary[Arrival] = {
    Arbitrary {
      for {
        arrivalId               <- arbitrary[ArrivalId]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        eoriNumber              <- arbitrary[String]
        status                  <- arbitrary[ArrivalStatus]
        created                 <- arbitrary[LocalDateTime]
        updated                 <- arbitrary[LocalDateTime]
        messages                <- nonEmptyListOfMaxLength[MovementMessageWithStatus](2)
      } yield
        Arrival(
          arrivalId = arrivalId,
          movementReferenceNumber = movementReferenceNumber,
          eoriNumber = eoriNumber,
          status = status,
          created = created,
          updated = updated,
          lastUpdated = LocalDateTime.now,
          messages = messages,
          nextMessageCorrelationId = messages.length + 1
        )
    }
  }

  implicit lazy val arbitraryMovementReferenceNumber: Arbitrary[MovementReferenceNumber] =
    Arbitrary {
      for {
        year    <- Gen.choose(0, 99).map(y => f"$y%02d")
        country <- Gen.pick(2, 'A' to 'Z')
        serial  <- Gen.pick(13, ('A' to 'Z') ++ ('0' to '9'))
      } yield MovementReferenceNumber(year ++ country.mkString ++ serial.mkString)
    }

  implicit lazy val arbitraryRejectionError: Arbitrary[RejectionError] =
    Arbitrary {

      for {
        errorType     <- arbitrary[Int]
        pointer       <- arbitrary[String]
        reason        <- arbitrary[Option[String]]
        originalValue <- arbitrary[Option[String]]
      } yield RejectionError(errorType, pointer, reason, originalValue)
    }

  implicit lazy val arbitraryMessageType: Arbitrary[MessageType] =
    Arbitrary(Gen.oneOf(MessageType.values))

  implicit lazy val arbitrarySubmissionResult: Arbitrary[SubmissionProcessingResult] =
    Arbitrary(Gen.oneOf(SubmissionProcessingResult.values))

  implicit lazy val arbitraryMessageReceived: Arbitrary[MessageReceivedEvent] =
    Arbitrary(Gen.oneOf(MessageReceivedEvent.values))

  implicit lazy val arbitraryMessageId: Arbitrary[MessageId] =
    Arbitrary {
      intsAboveValue(0).map(MessageId.fromIndex)
    }

  implicit lazy val arbitraryFailure: Arbitrary[SubmissionFailure] =
    Arbitrary(Gen.oneOf(SubmissionFailureInternal, SubmissionFailureExternal))
}
