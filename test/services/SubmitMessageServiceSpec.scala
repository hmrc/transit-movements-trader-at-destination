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
import cats.data.NonEmptyList
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionFailureDownstream
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionSuccessful
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import connectors.MessageConnector.EisSubmissionResult.VirusFoundOrInvalidToken
import generators.ModelGenerators
import models.ChannelType.web
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.Arrival
import models.ArrivalId
import models.ArrivalIdSelector
import models.ArrivalPutUpdate
import models.CompoundStatusUpdate
import models.MessageId
import models.MessageSelector
import models.MessageStatus
import models.MessageStatusUpdate
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionFailureRejected
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository
import utils.Format

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SubmitMessageServiceSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val requestXmlBody =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>MRN</DocNumHEA5>
      </HEAHEA>
    </CC007A>

  val messageId = MessageId(1)

  val movementMessage = MovementMessageWithStatus(
    messageId,
    localDateTime,
    MessageType.ArrivalNotification,
    requestXmlBody,
    SubmissionPending,
    2
  )

  val arrivalWithOneMessage: Gen[Arrival] = for {
    arrival <- arbitrary[Arrival]
  } yield
    arrival.copy(
      eoriNumber = "eori",
      messages = NonEmptyList.one(movementMessage),
      nextMessageCorrelationId = movementMessage.messageCorrelationId
    )

  "submit a new message" - {
    "return SubmissionSuccess and set the message status to submitted when the message is successfully saved, submitted" in {
      lazy val mockArrivalMovementRepository: ArrivalMovementRepository = mock[ArrivalMovementRepository]
      lazy val mockMessageConnector: MessageConnector                   = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess

        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(
          eqTo(ArrivalIdSelector(arrivalId)),
          eqTo(CompoundStatusUpdate(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded)))
        )(any())

      }
    }

    "return SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Failure(new Throwable())))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(
          eqTo(ArrivalIdSelector(arrivalId)),
          eqTo(CompoundStatusUpdate(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded)))
        )(any())
      }

    }

    "return SubmissionFailureInternal when the message is not saved" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Failure(new Exception)))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockMessageConnector, never()).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
      }

    }

    "return SubmissionFailureExternal and set the message status to SubmissionFailed when the message successfully saves, but the external service fails on submission" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        forAll(
          arbitrary[ArrivalId],
          arbitrary[MessageId],
          arbitrary[MovementMessageWithStatus],
          arbitrary[EisSubmissionFailureDownstream]
        ) {
          (arrivalId, messageId, movementMessage, submissionFailure) =>
            when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
            when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(submissionFailure))
            when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

            val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

            val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

            result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal

            verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
            verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
            verify(mockArrivalMovementRepository, times(1)).updateArrival(any(), eqTo(expectedModifier))(any())

        }
      }
    }

    "return SubmissionFailureRejected and set the message status to SubmissionFailed when the message successfully saves, but the external service rejects the message" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value

        when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

        val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)

        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(any(), eqTo(expectedModifier))(any())
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS due to virus found or invalid token" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureRejected if there has been a rejection from EIS due to error in payload" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage, web)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)
      }
    }
  }

  "submit a IE007 message" - {
    "return SubmissionSuccess and set the message status to submitted when the message is successfully saved, submitted" in {
      lazy val mockArrivalMovementRepository: ArrivalMovementRepository = mock[ArrivalMovementRepository]
      lazy val mockMessageConnector: MessageConnector                   = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value
        val mrn             = arbitrary[MovementReferenceNumber].sample.value

        val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess

        val expectedSelector = ArrivalIdSelector(arrivalId)
        val expectedModifier =
          ArrivalPutUpdate(mrn, CompoundStatusUpdate(MessageStatusUpdate(messageId, SubmissionSucceeded)))

        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(eqTo(expectedSelector), eqTo(expectedModifier))(any())

      }

    }

    "return SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))
      when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Failure(new Exception)))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value
        val mrn             = arbitrary[MovementReferenceNumber].sample.value

        val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

        val expectedSelector = ArrivalIdSelector(arrivalId)
        val expectedModifier =
          ArrivalPutUpdate(mrn, CompoundStatusUpdate(MessageStatusUpdate(messageId, SubmissionSucceeded)))

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(eqTo(expectedSelector), eqTo(expectedModifier))(any())
      }

    }

    "return SubmissionFailureInternal when the message is not saved" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Failure(new Exception)))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value
        val mrn             = arbitrary[MovementReferenceNumber].sample.value

        val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockMessageConnector, never()).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
      }

    }

    "return SubmissionFailureExternal and set the message status to SubmissionFailed when the message successfully saves, but the external service fails on submission" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        forAll(
          arbitrary[ArrivalId],
          arbitrary[MessageId],
          arbitrary[MovementMessageWithStatus],
          arbitrary[MovementReferenceNumber],
          arbitrary[EisSubmissionFailureDownstream]
        ) {
          (arrivalId, messageId, movementMessage, mrn, submissionFailure) =>
            when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
            when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(submissionFailure))
            when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

            val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

            val expectedSelector = MessageSelector(arrivalId, messageId)
            val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

            result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal
            verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
            verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
            verify(mockArrivalMovementRepository, times(1)).updateArrival(eqTo(expectedSelector), eqTo(expectedModifier))(any())
        }
      }

    }

    "return SubmissionFailureRejected and set the message status to SubmissionFailed when the message successfully saves, but the external service rejects the message" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value
        val mrn             = arbitrary[MovementReferenceNumber].sample.value

        when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

        val expectedSelector = MessageSelector(arrivalId, messageId)
        val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)

        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(eqTo(expectedSelector), eqTo(expectedModifier))(any())
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS due to virus found or invalid token" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value
        val mrn             = arbitrary[MovementReferenceNumber].sample.value

        val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureRejected if there has been a rejection from EIS due to error in payload" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val arrivalId       = arbitrary[ArrivalId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MovementMessageWithStatus].sample.value
        val mrn             = arbitrary[MovementReferenceNumber].sample.value

        val result = service.submitIe007Message(arrivalId, messageId, movementMessage, mrn, web)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)
      }
    }
  }

  "submit a new arrival" - {
    val arrivalWithOneMovementGenerator = arbitrary[Arrival].map(_.copy(messages = NonEmptyList.one(movementMessage)))
    val arrival                         = arrivalWithOneMovementGenerator.sample.value

    "return SubmissionSuccess and set the message status to submitted when the message is successfully saved, submitted" in {
      lazy val mockArrivalMovementRepository: ArrivalMovementRepository = mock[ArrivalMovementRepository]
      lazy val mockMessageConnector: MessageConnector                   = mock[MessageConnector]

      when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))
      when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess

        verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
        verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(
          eqTo(ArrivalIdSelector(arrival.arrivalId)),
          eqTo(CompoundStatusUpdate(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded)))
        )(any())

      }
    }

    "return SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))
      when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Failure(new Throwable())))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
        verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(
          eqTo(ArrivalIdSelector(arrival.arrivalId)),
          eqTo(CompoundStatusUpdate(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded)))
        )(any())
      }

    }

    "return SubmissionFailureInternal when the message is not saved" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.failed(new Exception))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockMessageConnector, never()).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any(), any())(any())
      }

    }

    "return SubmissionFailureExternal and set the message status to SubmissionFailed when the message successfully saves, but the external service fails on submission" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        forAll(arrivalWithOneMovementGenerator, arbitrary[EisSubmissionFailureDownstream]) {
          (arrival, submissionResult) =>
            Mockito.reset(mockArrivalMovementRepository, mockMessageConnector)

            when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
            when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(submissionResult))
            when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

            val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

            val result = service.submitArrival(arrival)

            result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal
            verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
            verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any(), any())(any())
            verify(mockArrivalMovementRepository, times(1)).updateArrival(any(), eqTo(expectedModifier))(any())
        }
      }
    }

    "return SubmissionFailureRejected and set the message status to SubmissionFailed when the message successfully saves, but the external service rejects the message" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)

        verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
        verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any(), any())(any())
        verify(mockArrivalMovementRepository, times(1)).updateArrival(any(), eqTo(expectedModifier))(any())
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS due to virus found or invalid token" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureRejected if there has been a rejection from EIS due to error in payload" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockArrivalMovementRepository.updateArrival(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)
      }
    }

  }
}
