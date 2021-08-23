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

import base.SpecBase
import generators.ModelGenerators
import models.MessageType.GoodsReleased
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import java.time.LocalDateTime
import java.time.LocalDateTime
import play.api.libs.json.JsObject

class ArrivalMessageNotificationSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  val responseGenerator = Gen.oneOf(MessageResponse.inboundMessages)

  "fromArrivalNotification" - {

    "must convert InboundMessageRequest to ArrivalNotification and includes message body" in {

      val arrival     = arbitraryArrival.arbitrary.sample.value
      val dateTimeNow = LocalDateTime.now()

      val requestXml = <CC024A></CC024A>

      val expectedNotification =
        ArrivalMessageNotification(
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}/messages/${arrival.messages.length + 1}",
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}",
          arrival.eoriNumber,
          arrival.arrivalId,
          MessageId(arrival.messages.length + 1),
          dateTimeNow,
          GoodsReleased,
          Some(requestXml)
        )

      val inboundMessageRequest = InboundMessageRequest(
        arrival = arrival,
        nextStatus = ArrivalStatus.GoodsReleased,
        inboundMessageResponse = GoodsReleasedResponse,
        movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          dateTimeNow,
          GoodsReleased,
          requestXml,
          0,
          JsObject.empty
        )
      )

      val result = ArrivalMessageNotification.fromInboundRequest(inboundMessageRequest, Some(1))

      result mustEqual expectedNotification
    }

    "must convert InboundMessageRequest to ArrivalNotification and does not include message body over 100kb" in {

      val arrival     = arbitraryArrival.arbitrary.sample.value
      val dateTimeNow = LocalDateTime.now()

      val requestXml = <CC024A></CC024A>

      val expectedNotification =
        ArrivalMessageNotification(
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}/messages/${arrival.messages.length + 1}",
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}",
          arrival.eoriNumber,
          arrival.arrivalId,
          MessageId(arrival.messages.length + 1),
          dateTimeNow,
          GoodsReleased,
          None
        )

      val inboundMessageRequest = InboundMessageRequest(
        arrival = arrival,
        nextStatus = ArrivalStatus.GoodsReleased,
        inboundMessageResponse = GoodsReleasedResponse,
        movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          dateTimeNow,
          GoodsReleased,
          requestXml,
          0,
          JsObject.empty
        )
      )

      val result = ArrivalMessageNotification.fromInboundRequest(inboundMessageRequest, Some(100001))

      result mustEqual expectedNotification
    }
  }
}
