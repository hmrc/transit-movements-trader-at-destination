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

package services

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import base.SpecBase
import connectors.MessageConnector
import generators.ModelGenerators
import models.MessageState.SubmissionPending
import models.Arrival
import models.ArrivalId
import models.ArrivalState
import models.MessageId
import models.MessageState
import models.MessageType
import models.MovementMessageWithState
import models.SubmissionResult
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito.when
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import play.api.inject.bind
import play.api.test.Helpers.ACCEPTED
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HttpResponse
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SubmitMessageServiceSpec extends SpecBase with ModelGenerators {

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

  val messageId = new MessageId(0)

  val movementMessage = MovementMessageWithState(
    localDateTime,
    MessageType.ArrivalNotification,
    requestXmlBody,
    SubmissionPending,
    2
  )

  val arrivalWithOneMessage: Gen[Arrival] = for {
    arrival <- arbitrary[Arrival]
  } yield {
    arrival.copy(eoriNumber = "eori",
                 status = ArrivalState.ArrivalSubmitted,
                 messages = Seq(movementMessage),
                 nextMessageCorrelationId = movementMessage.messageCorrelationId)
  }

  "submit a new message" - {
    "return SubmissionResult.Success when the message is successfully saved, submitted and the state is updated" in {
      lazy val mockArrivalMovementRepository: ArrivalMovementRepository = mock[ArrivalMovementRepository]
      lazy val mockMessageConnector: MessageConnector                   = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockArrivalMovementRepository.setArrivalStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(Some(())))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))

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
        val movementMessage = arbitrary[MovementMessageWithState].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.Success

        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setArrivalStateAndMessageState(eqTo(arrivalId),
                                                                                       eqTo(messageId),
                                                                                       eqTo(ArrivalState.ArrivalSubmitted),
                                                                                       eqTo(MessageState.SubmissionSucceeded))

      }

    }

    "return SubmissionResult.Success when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockArrivalMovementRepository.setArrivalStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(None))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))

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
        val movementMessage = arbitrary[MovementMessageWithState].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.Success
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setArrivalStateAndMessageState(eqTo(arrivalId),
                                                                                       eqTo(messageId),
                                                                                       eqTo(ArrivalState.ArrivalSubmitted),
                                                                                       eqTo(MessageState.SubmissionSucceeded))
      }

    }

    "return SubmissionResult.FailureInternal when the message is not saved" in {
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
        val movementMessage = arbitrary[MovementMessageWithState].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.FailureInternal
        verify(mockMessageConnector, never()).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
      }

    }

    "return SubmissionResult.FailureExternal when the message successfully saves, but is not submitted and set the message state to SubmissionFailed" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.failed(new Exception))
      when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful((Success(()))))

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
        val movementMessage = arbitrary[MovementMessageWithState].sample.value

        val result = service.submitMessage(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.FailureExternal
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setMessageState(eqTo(arrivalId), eqTo(messageId.index), eqTo(MessageState.SubmissionFailed))
      }

    }

  }

  "submit a new arrival" - {
    val arrival = arbitrary[Arrival].sample.value.copy(messages = Seq(movementMessage))

    "return SubmissionResult.Success when the message is successfully saved, submitted and the state is updated" in {
      lazy val mockArrivalMovementRepository: ArrivalMovementRepository = mock[ArrivalMovementRepository]
      lazy val mockMessageConnector: MessageConnector                   = mock[MessageConnector]

      when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
      when(mockArrivalMovementRepository.setArrivalStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(Some(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionResult.Success

        verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
        verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setArrivalStateAndMessageState(
          eqTo(arrival.arrivalId),
          eqTo(messageId),
          eqTo(ArrivalState.ArrivalSubmitted),
          eqTo(MessageState.SubmissionSucceeded)
        )

      }
    }

    "return SubmissionResult.Success when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
      when(mockArrivalMovementRepository.setArrivalStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(None))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionResult.Success
        verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
        verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setArrivalStateAndMessageState(eqTo(arrival.arrivalId),
                                                                                       eqTo(messageId),
                                                                                       eqTo(ArrivalState.ArrivalSubmitted),
                                                                                       eqTo(MessageState.SubmissionSucceeded))
      }

    }

    "return SubmissionResult.FailureInternal when the message is not saved" in {
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

        result.futureValue mustEqual SubmissionResult.FailureInternal
        verify(mockMessageConnector, never()).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any())(any())
      }

    }

    "return SubmissionResult.FailureExternal when the message successfully saves, but is not submitted and set the message state to SubmissionFailed" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.failed(new Exception))
      when(mockArrivalMovementRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful((Success(()))))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitArrival(arrival)

        result.futureValue mustEqual SubmissionResult.FailureExternal
        verify(mockArrivalMovementRepository, times(1)).insert(eqTo(arrival))
        verify(mockMessageConnector, times(1)).post(eqTo(arrival.arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setMessageState(eqTo(arrival.arrivalId), eqTo(messageId.index), eqTo(MessageState.SubmissionFailed))
      }

    }

  }

}
