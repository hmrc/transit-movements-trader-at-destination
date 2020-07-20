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
import play.api.inject.bind
import play.core.server.common.WebSocketFlowHandler.MessageType

class MessageRetrievalServiceSpec extends SpecBase with ModelGenerators {

  private val mockArrivalMessageSummaryService = mock[ArrivalMessageSummaryService]

  "MessageRetrievalService" - {

    "return UnloadingPermission" in {

      val unloadingPermission = MovementMessageWithStatus(LocalDateTime.now(), UnloadingPermission, <blankXml>message</blankXml>, SubmissionSucceeded, 1)
      val genArrival          = arbitrary[Arrival].sample.value
      val woa                 = genArrival.copy(messages = NonEmptyList(unloadingPermission, List.empty))

      val arrivalSummary = MessagesSummary(
        arrivalNotification = MessageId.fromIndex(1),
        unloadingPermission = Some(MessageId.fromIndex(0)),
        arrival = woa,
      )

      when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any()))
        .thenReturn(arrivalSummary)

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)
        )
        .build()

      val service = application.injector.instanceOf[MessageRetrievalService]

      service.getUnloadingPermission(woa) mustBe Some(unloadingPermission)
    }

    "return None when UnloadingPermission MessageId cannot be found" in {}

    "return None when UnloadingPermission cannot be found" in {}

    "return None when UnloadingPermission is in a Failed state" in {}

  }
}
