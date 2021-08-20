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
import controllers.routes
import generators.ModelGenerators
import models.ChannelType.api
import models.request.ArrivalRequest
import models.request.ArrivalWithoutMessagesRequest
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.HeaderNames
import play.api.http.HttpVerbs
import play.api.test.FakeRequest

import java.time.LocalDateTime

import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

import scala.xml.NodeSeq

class ArrivalMessageNotificationSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with HttpVerbs {

  val responseGenerator = Gen.oneOf(MessageResponse.inboundMessages)

  "fromArrival" - {

    "produces the expected model" in {

      val arrival     = arbitraryArrival.arbitrary.sample.value
      val messageType = Gen.oneOf(MessageType.values).sample.value
      val dateTimeNow = LocalDateTime.now()

      val requestXml = <text></text>

      val expectedNotification =
        ArrivalMessageNotification(
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}/messages/${arrival.messages.length + 1}",
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}",
          arrival.eoriNumber,
          arrival.arrivalId,
          MessageId(arrival.messages.length + 1),
          dateTimeNow,
          messageType,
          Some(requestXml)
        )

      val testNotification = ArrivalMessageNotification.fromArrival(arrival, dateTimeNow, messageType, requestXml, Some(123))

      testNotification mustEqual expectedNotification
    }

    "does not include the message body when it is over 100kb" in {

      val arrival     = arbitraryArrival.arbitrary.sample.value
      val messageType = Gen.oneOf(MessageType.values).sample.value
      val dateTimeNow = LocalDateTime.now()

      val requestXml = <text></text>

      val expectedNotification =
        ArrivalMessageNotification(
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}/messages/${arrival.messages.length + 1}",
          s"/customs/transits/movements/arrivals/${arrival.arrivalId.index}",
          arrival.eoriNumber,
          arrival.arrivalId,
          MessageId(arrival.messages.length + 1),
          dateTimeNow,
          messageType,
          None
        )

      val testNotification = ArrivalMessageNotification.fromArrival(arrival, dateTimeNow, messageType, requestXml, Some(100001))

      testNotification mustEqual expectedNotification
    }
  }
}
