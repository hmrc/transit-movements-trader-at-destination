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

package controllers

import base.SpecBase
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.FakeAuthenticatedGetArrivalForReadActionProvider
import generators.ModelGenerators
import models.Arrival
import models.MessageId
import models.MessagesSummary
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.ArrivalMessageSummaryService

class MessagesSummaryControllerSpec extends SpecBase with ModelGenerators {

  val arrival: Arrival = arbitrary[Arrival].sample.value
  val arrivalId        = arrival.arrivalId

  "messagesSummary" - {
    "return an OK with the message summary when there is a matching arrival" in {
      val fake        = FakeAuthenticatedGetArrivalForReadActionProvider(arrival)
      val mockService = mock[ArrivalMessageSummaryService]

      val arrivalNotificationMessageId = MessageId.fromMessageIdValue(1).value
      val arrivalRejectionMessageId    = MessageId.fromMessageIdValue(2).value
      val messagesSummary              = MessagesSummary(arrival, arrivalNotificationMessageId, Some(arrivalRejectionMessageId))

      when(mockService.arrivalMessagesSummary(any())).thenReturn(messagesSummary)

      val app =
        baseApplicationBuilder
          .overrides(
            bind[AuthenticatedGetArrivalForReadActionProvider].toInstance(fake),
            bind[ArrivalMessageSummaryService].toInstance(mockService)
          )
          .build()

      running(app) {
        val request = FakeRequest(GET, routes.MessagesSummaryController.messagesSummary(arrivalId).url)

        val result = route(app, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJsObject(messagesSummary)
      }
    }
  }

}
