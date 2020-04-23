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

import base.SpecBase
import connectors.MessageConnector
import generators.ModelGenerators
import models.ArrivalId
import models.ArrivalState
import models.MessageId
import models.MessageState
import models.MovementMessageWithState
import models.SubmissionResult
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito.when
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import play.api.inject.bind
import play.api.test.Helpers.ACCEPTED
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SubmitMessageServiceSpec extends SpecBase with ModelGenerators {

  "submit" - {
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

        val result = service.submit(arrivalId, messageId, movementMessage)

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

        val result = service.submit(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.Success
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setArrivalStateAndMessageState(eqTo(arrivalId),
                                                                                       eqTo(messageId),
                                                                                       eqTo(ArrivalState.ArrivalSubmitted),
                                                                                       eqTo(MessageState.SubmissionSucceeded))
      }

    }

    "return SubmissionResult.Success when the message is successfully saved and submitted and " ++
      "the state of the arrival is not updated, then the message state should not be updated to Failed" ignore {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockArrivalMovementRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
      when(mockArrivalMovementRepository.setState(any(), any())).thenReturn(Future.successful(None))

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

        val result = service.submit(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.Success
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, never()).setArrivalStateAndMessageState(any(), any(), any(), any())
      }

    }

    "return SubmissionResult.Failure when the message is not saved" in {
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

        val result = service.submit(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.Failure
        verify(mockMessageConnector, never()).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
      }

    }

    "return SubmissionResult.Failure when the message successfully saves, but is not submitted and set the message state to SubmissionFailed" in {
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

        val result = service.submit(arrivalId, messageId, movementMessage)

        result.futureValue mustEqual SubmissionResult.Failure
        verify(mockArrivalMovementRepository, times(1)).addNewMessage(eqTo(arrivalId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(arrivalId), eqTo(movementMessage), any())(any())
        verify(mockArrivalMovementRepository, times(1)).setMessageState(eqTo(arrivalId), eqTo(messageId.index), eqTo(MessageState.SubmissionFailed))
      }

    }

  }
}
