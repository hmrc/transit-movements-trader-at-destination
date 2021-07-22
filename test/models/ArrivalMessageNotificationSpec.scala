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

package models

import java.time.LocalDateTime

import base.SpecBase
import controllers.actions.InboundMessageRequest
import controllers.routes
import generators.ModelGenerators
import models.ChannelType.api
import models.request.ArrivalRequest
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.HttpVerbs
import play.api.test.FakeRequest
import org.scalacheck.Arbitrary._
import scala.xml.NodeSeq

class ArrivalMessageNotificationSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with HttpVerbs {

  val responseGenerator = Gen.oneOf(MessageResponse.inboundMessages)

  "fromRequest" - {
    "produces the expected model" in {
      val response      = responseGenerator.sample.value
      val arrival       = arbitraryArrival.arbitrary.sample.value
      val messageSender = MessageSender(arrival.arrivalId, arrival.messages.last.messageCorrelationId)

      val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
        .withBody[NodeSeq](<text></text>)
      val arrivalRequest = ArrivalRequest(request, arrival, api)
      val inboundRequest = InboundMessageRequest(InboundMessage(response, arbitrary[ArrivalStatus].sample.value), arrivalRequest)

      val now = LocalDateTime.now()

      val expectedNotification =
        ArrivalMessageNotification(
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}/messages/${arrival.messages.length + 1}",
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}",
          arrival.arrivalId,
          MessageId(arrival.messages.length + 1),
          now,
          response.messageType
        )

      val testNotification = ArrivalMessageNotification.fromRequest(inboundRequest, now)

      testNotification mustEqual expectedNotification
    }
  }
}
