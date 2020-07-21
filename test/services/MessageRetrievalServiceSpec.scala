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

import java.time.LocalDateTime

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionSucceeded
import models.MessageType.UnloadingPermission
import models.Arrival
import models.MessageId
import models.MessagesSummary
import models.MovementMessageWithStatus
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind

class MessageRetrievalServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockArrivalMessageSummaryService = mock[ArrivalMessageSummaryService]

  "MessageRetrievalService" - {

    "getUnloadingPermission" - {

      "must return UnloadingPermission" in {

        val unloadingPermission = MovementMessageWithStatus(LocalDateTime.now(), UnloadingPermission, <blankXml>message</blankXml>, SubmissionSucceeded, 1)
        val genArrival = arbitrary[Arrival].sample.value
        val arrivalWithUnloadingPermission = genArrival.copy(messages = NonEmptyList(unloadingPermission, List.empty))

        val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(1), unloadingPermission = Some(MessageId.fromIndex(0)), arrival = arrivalWithUnloadingPermission)

        when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

        val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

        val service = application.injector.instanceOf[MessageRetrievalService]

        val expectedResult = ResponseMovementMessage.build(arrivalWithUnloadingPermission.arrivalId, MessageId.fromIndex(0), unloadingPermission)

        service.getUnloadingPermission(arrivalWithUnloadingPermission).value mustBe expectedResult
      }

      "must return None when UnloadingPermission MessageId cannot be found" in {
        val unloadingPermission = MovementMessageWithStatus(LocalDateTime.now(), UnloadingPermission, <blankXml>message</blankXml>, SubmissionSucceeded, 1)
        val genArrival = arbitrary[Arrival].sample.value
        val arrivalWithUnloadingPermission = genArrival.copy(messages = NonEmptyList(unloadingPermission, List.empty))

        val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(1), arrival = arrivalWithUnloadingPermission)

        when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

        val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

        val service = application.injector.instanceOf[MessageRetrievalService]

        service.getUnloadingPermission(arrivalWithUnloadingPermission) mustBe None

      }

      "must return None when UnloadingPermission cannot be found" in {
        val genArrival = arbitrary[Arrival].sample.value
        val genMovementMessage = arbitrary[MovementMessageWithStatus].suchThat(_.messageType != UnloadingPermission).sample.value
        val arrivalWithoutUnloadingPermission = genArrival.copy(messages = NonEmptyList(genMovementMessage, List.empty))

        val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(0), unloadingPermission = Some(MessageId.fromIndex(1)), arrival = arrivalWithoutUnloadingPermission)

        when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

        val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

        val service = application.injector.instanceOf[MessageRetrievalService]

        service.getUnloadingPermission(arrivalWithoutUnloadingPermission) mustBe None
      }

      "must return None when UnloadingPermission is in a Failed state" in {
        val unloadingPermission = MovementMessageWithStatus(LocalDateTime.now(), UnloadingPermission, <blankXml>message</blankXml>, SubmissionFailed, 1)
        val genArrival = arbitrary[Arrival].sample.value
        val arrivalWithUnloadingPermission = genArrival.copy(messages = NonEmptyList(unloadingPermission, List.empty))

        val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(1), unloadingPermission = Some(MessageId.fromIndex(0)), arrival = arrivalWithUnloadingPermission)

        when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

        val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

        val service = application.injector.instanceOf[MessageRetrievalService]

        service.getUnloadingPermission(arrivalWithUnloadingPermission) mustBe None
      }
    }
  }

}
