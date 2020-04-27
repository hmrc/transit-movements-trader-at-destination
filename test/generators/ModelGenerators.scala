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

import models.MessageState.SubmissionPending
import models.Arrival
import models.ArrivalId
import models.ArrivalState
import models.MessageId
import models.MessageReceived
import models.MessageType
import models.MovementMessageWithState
import models.MovementMessageWithoutState
import models.MovementReferenceNumber
import models.RejectionError
import models.SubmissionResult
import models.SubmissionResult._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait ModelGenerators extends BaseGenerators with JavaTimeGenerators {

  private val pastDate: LocalDate = LocalDate.of(1900, 1, 1)
  private val dateNow: LocalDate  = LocalDate.now

  implicit lazy val arbitraryMessageWithStateXml: Arbitrary[MovementMessageWithState] = {
    Arbitrary {
      for {
        dateTime    <- arbitrary[LocalDateTime]
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
        state = SubmissionPending
      } yield MovementMessageWithState(dateTime, messageType, xml, state, 1)
    }
  }

  implicit lazy val arbitraryMessageWithoutStateXml: Arbitrary[MovementMessageWithoutState] = {
    Arbitrary {
      for {
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
      } yield MovementMessageWithoutState(LocalDateTime.of(date, time), messageType, xml, 1)
    }
  }

  implicit lazy val arbitraryArrivalId: Arbitrary[ArrivalId] = {
    Arbitrary {
      for {
        id <- arbitrary[Int]
      } yield ArrivalId(id)
    }
  }

  implicit lazy val arbitraryState: Arbitrary[ArrivalState] =
    Arbitrary {
      Gen.oneOf(ArrivalState.values)
    }

  implicit lazy val arbitraryArrival: Arbitrary[Arrival] = {
    Arbitrary {
      for {
        arrivalId               <- arbitrary[ArrivalId]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        eoriNumber              <- arbitrary[String]
        state                   <- arbitrary[ArrivalState]
        created                 <- arbitrary[LocalDateTime]
        updated                 <- arbitrary[LocalDateTime]
        messages                <- listWithMaxLength[MovementMessageWithState](2)
      } yield
        Arrival(
          arrivalId = arrivalId,
          movementReferenceNumber = movementReferenceNumber,
          eoriNumber = eoriNumber,
          state = state,
          created = created,
          updated = updated,
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

  implicit lazy val arbitrarySubmissionResult: Arbitrary[SubmissionResult] =
    Arbitrary(Gen.oneOf(SubmissionResult.values))

  implicit lazy val arbitraryMessageReceived: Arbitrary[MessageReceived] =
    Arbitrary(Gen.oneOf(MessageReceived.values))

  implicit lazy val arbitraryMessageId: Arbitrary[MessageId] =
    Arbitrary {
      intsAboveValue(0).map(new MessageId(_))
    }

  implicit lazy val arbitraryFailure: Arbitrary[Failure] =
    Arbitrary(Gen.oneOf(FailureInternal, FailureExternal))
}
