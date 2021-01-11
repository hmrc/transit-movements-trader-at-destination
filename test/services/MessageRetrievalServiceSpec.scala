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
import generators.ModelGenerators
import models.MessageType.UnloadingPermission
import models.Arrival
import models.MessageId
import models.MessagesSummary
import models.MovementMessageWithoutStatus
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running

class MessageRetrievalServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockArrivalMessageSummaryService = mock[ArrivalMessageSummaryService]

  "MessageRetrievalService" - {
    "getUnloadingPermission" - {
      "must return UnloadingPermission" in {
        forAll(arbitrary[Arrival], arbitrary[MovementMessageWithoutStatus]) {
          (arrival, movementMessageWithStatus) =>
            val unloadingPermissionMessage     = movementMessageWithStatus.copy(messageType = UnloadingPermission)
            val arrivalWithUnloadingPermission = arrival.copy(messages = NonEmptyList(movementMessageWithStatus, List(unloadingPermissionMessage)))

            val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(0),
                                                 unloadingPermission = Some(MessageId.fromIndex(1)),
                                                 arrival = arrivalWithUnloadingPermission)

            when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

            val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

            running(application) {
              val service = application.injector.instanceOf[MessageRetrievalService]

              val expectedResult = ResponseMovementMessage.build(arrivalWithUnloadingPermission.arrivalId, MessageId.fromIndex(1), unloadingPermissionMessage)

              service.getUnloadingPermission(arrivalWithUnloadingPermission).value mustBe expectedResult
            }
        }
      }

      "must return None when UnloadingPermission MessageId cannot be found" in {
        forAll(arbitrary[Arrival]) {
          arrival =>
            val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(0), unloadingPermission = None, arrival = arrival)

            when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

            val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

            running(application) {
              val service = application.injector.instanceOf[MessageRetrievalService]

              service.getUnloadingPermission(arrival) mustBe None
            }
        }
      }

      "must return None when MessageId is found and UnloadingPermission is not defined" in {
        forAll(arbitrary[Arrival], arbitrary[MovementMessageWithoutStatus]) {
          (arrival, movementMessageWithStatus) =>
            val arrivalWithoutUnloadingPermission = arrival.copy(messages = NonEmptyList(movementMessageWithStatus, List.empty))

            val arrivalSummary = MessagesSummary(arrivalNotification = MessageId.fromIndex(0),
                                                 unloadingPermission = Some(MessageId.fromIndex(1)),
                                                 arrival = arrivalWithoutUnloadingPermission)

            when(mockArrivalMessageSummaryService.arrivalMessagesSummary(any())).thenReturn(arrivalSummary)

            val application = baseApplicationBuilder.overrides(bind[ArrivalMessageSummaryService].toInstance(mockArrivalMessageSummaryService)).build()

            running(application) {
              val service = application.injector.instanceOf[MessageRetrievalService]

              service.getUnloadingPermission(arrivalWithoutUnloadingPermission) mustBe None
            }
        }
      }
    }
  }
}
