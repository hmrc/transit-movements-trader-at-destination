/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.data.NonEmptyList
import config.Constants
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult.DownstreamBadGateway
import connectors.MessageConnector.EisSubmissionResult.DownstreamInternalServerError
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionFailure
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionFailureDownstream
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionRejected
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionSuccessful
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import connectors.MessageConnector.EisSubmissionResult.UnexpectedHttpResponse
import connectors.MessageConnector.EisSubmissionResult.VirusFoundOrInvalidToken
import models.Arrival
import models.ArrivalId
import models.ArrivalMessages
import models.ArrivalPutUpdate
import models.ArrivalStatus
import models.ArrivalUpdate
import models.ArrivalWithoutMessages
import models.Box
import models.BoxId
import models.ChannelType
import models.MessageId
import models.MessageMetaData
import models.MessageReceivedEvent
import models.MessageStatus
import models.MessageStatusUpdate
import models.MessageType
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import models.RejectionError
import models.SubmissionProcessingResult
import models.response.ResponseArrival
import models.response.ResponseMovementMessage
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.http.UpstreamErrorResponse

import java.time._

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

  implicit val arbitraryArrivalPutUpdate: Arbitrary[ArrivalPutUpdate] = Arbitrary {
    for {
      mrn    <- arbitrary[MovementReferenceNumber]
      update <- arbitrary[MessageStatusUpdate]
    } yield ArrivalPutUpdate(mrn, update)
  }

  val arrivalUpdateTypeGenerator: Gen[Gen[ArrivalUpdate]] =
    Gen.oneOf[Gen[ArrivalUpdate]](
      arbitrary[MessageStatusUpdate],
      arbitrary[ArrivalPutUpdate]
    )

  implicit val arbitraryArrivalUpdate: Arbitrary[ArrivalUpdate] =
    Arbitrary(
      Gen.oneOf[ArrivalUpdate](
        arbitrary[MessageStatusUpdate],
        arbitrary[ArrivalPutUpdate]
      )
    )

  implicit lazy val arbitraryMessageStatus: Arbitrary[MessageStatus] =
    Arbitrary {
      Gen.oneOf(MessageStatus.values)
    }

  implicit lazy val arbitraryMessageWithStateXml: Arbitrary[MovementMessageWithStatus] =
    Arbitrary {
      for {
        messageId   <- arbitrary[MessageId]
        dateTime    <- arbitrary[LocalDateTime]
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
        status = MessageStatus.SubmissionPending
      } yield MovementMessageWithStatus(messageId, dateTime, Some(dateTime), messageType, xml, status, 1)
    }

  implicit lazy val arbitraryMessageWithoutStateXml: Arbitrary[MovementMessageWithoutStatus] =
    Arbitrary {
      for {
        messageId   <- arbitrary[MessageId]
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
      } yield MovementMessageWithoutStatus(messageId, LocalDateTime.of(date, time), Some(LocalDateTime.of(date, time)), messageType, xml, 1)
    }

  implicit lazy val arbitraryMovementMessage: Arbitrary[MovementMessage] =
    Arbitrary {
      Gen.oneOf(arbitrary[MovementMessageWithoutStatus], arbitrary[MovementMessageWithStatus])
    }

  implicit lazy val arbitraryArrivalId: Arbitrary[ArrivalId] =
    Arbitrary {
      for {
        id <- intWithMaxLength(9)
      } yield ArrivalId(id)
    }

  implicit lazy val arbitraryState: Arbitrary[ArrivalStatus] =
    Arbitrary {
      Gen.oneOf(ArrivalStatus.values)
    }

  implicit lazy val arbitraryChannel: Arbitrary[ChannelType] =
    Arbitrary {
      Gen.oneOf(ChannelType.values)
    }

  implicit lazy val arbitraryBoxId: Arbitrary[BoxId] =
    Arbitrary {
      Gen.uuid.map(_.toString).map(BoxId.apply)
    }

  implicit lazy val arbitraryBox: Arbitrary[Box] =
    Arbitrary {
      for {
        boxId <- arbitrary[BoxId]
      } yield Box(boxId, Constants.BoxName)
    }

  implicit lazy val arbitraryArrival: Arbitrary[Arrival] =
    Arbitrary {
      for {
        arrivalId               <- arbitrary[ArrivalId]
        channel                 <- arbitrary[ChannelType]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        eoriNumber              <- arbitrary[String]
        created                 <- arbitrary[LocalDateTime]
        updated                 <- arbitrary[LocalDateTime]
        messages                <- nonEmptyListOfMaxLength[MovementMessageWithStatus](2)
      } yield
        Arrival(
          arrivalId = arrivalId,
          channel = channel,
          movementReferenceNumber = movementReferenceNumber,
          eoriNumber = eoriNumber,
          created = created,
          updated = updated,
          lastUpdated = updated,
          messages = messages,
          nextMessageCorrelationId = messages.length + 1,
          notificationBox = None
        )
    }

  implicit lazy val arbitraryArrivalWithoutMessages: Arbitrary[ArrivalWithoutMessages] =
    Arbitrary(ArrivalWithoutMessages.fromArrival(arbitraryArrival.arbitrary.sample.get))

  val genArrivalWithSuccessfulArrival: Gen[Arrival] =
    Arbitrary {
      for {
        message <- Arbitrary.arbitrary[MovementMessageWithStatus]
        arrival <- Arbitrary.arbitrary[Arrival]
      } yield {
        val successfulMessage = message.copy(status = MessageStatus.SubmissionSucceeded)
        arrival.copy(messages = NonEmptyList.one(successfulMessage), eoriNumber = "eori")
      }
    }.arbitrary

  val genArrivalMessagesWithSpecificEori: Gen[ArrivalMessages] =
    Arbitrary {
      for {
        arrivalMessages <- Arbitrary.arbitrary[ArrivalMessages]
      } yield arrivalMessages.copy(eoriNumber = "eori")
    }.arbitrary

  implicit lazy val arbitraryMovementReferenceNumber: Arbitrary[MovementReferenceNumber] =
    Arbitrary {
      for {
        year <- Gen
          .choose(0, 99)
          .map(
            y => f"$y%02d"
          )
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
      intsAboveValue(0).map(MessageId.apply)
    }

  implicit lazy val arbitraryFailure: Arbitrary[SubmissionProcessingResult.SubmissionFailure] =
    Arbitrary(
      Gen.oneOf(
        SubmissionProcessingResult.SubmissionFailureInternal,
        SubmissionProcessingResult.SubmissionFailureExternal
      )
    )

  implicit lazy val arbitraryResponseMovementMessage: Arbitrary[ResponseMovementMessage] =
    Arbitrary {
      for {
        location    <- arbitrary[String]
        dateTime    <- arbitrary[LocalDateTime]
        messageType <- arbitrary[String]
        message     <- Gen.const(<blankXml>message</blankXml>)
      } yield ResponseMovementMessage(location, dateTime, messageType, message, Some(dateTime))
    }

  implicit lazy val arbitrarySubmissionFailure: Arbitrary[EisSubmissionFailure] =
    Arbitrary(Gen.oneOf(arbitrary[EisSubmissionRejected], arbitrary[EisSubmissionFailureDownstream]))

  implicit lazy val arbitrarySubmissionFailureInternal: Arbitrary[EisSubmissionRejected] =
    Arbitrary {
      Gen.oneOf(
        ErrorInPayload,
        VirusFoundOrInvalidToken
      )
    }

  implicit lazy val arbitrarySubmissionFailureDownstream: Arbitrary[EisSubmissionFailureDownstream] =
    Arbitrary {
      Gen.oneOf(
        DownstreamInternalServerError,
        DownstreamBadGateway,
        UnexpectedHttpResponse(UpstreamErrorResponse("", 418))
      )
    }

  implicit def arbitraryEisSubmissionResult: Arbitrary[EisSubmissionResult] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[EisSubmissionRejected],
        arbitrary[EisSubmissionFailureDownstream],
        Gen.const(EisSubmissionSuccessful)
      )
    )

  implicit val arbitraryMessageMetaData: Arbitrary[MessageMetaData] =
    Arbitrary(
      for {
        messageType <- arbitrary[MessageType]
        dateTime    <- arbitrary[LocalDateTime]
      } yield MessageMetaData(messageType, dateTime)
    )

  implicit val arbitraryResponseArrival: Arbitrary[ResponseArrival] =
    Arbitrary(
      for {
        arrivalId               <- arbitrary[ArrivalId]
        location                <- arbitrary[String]
        messagesLocation        <- arbitrary[String]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        status                  <- arbitrary[ArrivalStatus]
        created                 <- arbitrary[LocalDateTime]
        updated                 <- arbitrary[LocalDateTime]
      } yield ResponseArrival(arrivalId, location, messagesLocation, movementReferenceNumber, status, created, updated)
    )

  implicit lazy val arbitaryArrivalMessages: Arbitrary[ArrivalMessages] =
    Arbitrary(
      for {
        arrivalId <- arbitrary[ArrivalId]
        eori      <- arbitrary[String]
        message   <- arbitrary[MovementMessageWithStatus]
      } yield ArrivalMessages(arrivalId, eori, List(message))
    )

}
