/*
 * Copyright 2022 HM Revenue & Customs
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

import generators.ModelGenerators
import org.scalacheck.Arbitrary
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class MessagesSummarySpec extends AnyFreeSpec with Matchers with ModelGenerators with ScalaCheckPropertyChecks with OptionValues {

  private val arrival = Arbitrary.arbitrary[Arrival].sample.value

  "MessagesSummary" - {

    "return arrival link" in {

      val messageId = 1

      Json.toJson(
        MessagesSummary(arrival, MessageId(messageId), None, None)
      ) mustBe Json.obj(
        "arrivalId" -> arrival.arrivalId,
        "messages" ->
          Json.obj(
            "IE007" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$messageId"
          )
      )

    }

    "return arrival rejection link" in {

      val messageId            = 1
      val xmlNegativeMessageId = 2

      Json.toJson(
        MessagesSummary(arrival, MessageId(messageId), Some(MessageId(xmlNegativeMessageId)), None)
      ) mustBe Json.obj(
        "arrivalId" -> arrival.arrivalId,
        "messages" ->
          Json.obj(
            "IE007" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$messageId",
            "IE008" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$xmlNegativeMessageId"
          )
      )
    }

    "return xml Submission Negative Acknowledgement link" in {

      val messageId   = 1
      val rejectionId = 2

      Json.toJson(
        MessagesSummary(arrival = arrival, arrivalNotification = MessageId(messageId), xmlSubmissionNegativeAcknowledgement = Some(MessageId(rejectionId)))
      ) mustBe Json.obj(
        "arrivalId" -> arrival.arrivalId,
        "messages" ->
          Json.obj(
            "IE007" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$messageId",
            "IE917" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$rejectionId"
          )
      )
    }

    "return unloading permission link" in {

      val messageId             = 1
      val rejectionId           = 2
      val unloadingPermissionId = 3

      Json.toJson(
        MessagesSummary(arrival, MessageId(messageId), Some(MessageId(rejectionId)), Some(MessageId(unloadingPermissionId)))
      ) mustBe Json.obj(
        "arrivalId" -> arrival.arrivalId,
        "messages" ->
          Json.obj(
            "IE007" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$messageId",
            "IE008" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$rejectionId",
            "IE043" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$unloadingPermissionId"
          )
      )

    }

    "return unloading remarks link" in {

      val messageId             = 1
      val rejectionId           = 2
      val unloadingPermissionId = 3
      val unloadingRemarksId    = 4

      Json.toJson(
        MessagesSummary(
          arrival,
          MessageId(messageId),
          Some(MessageId(rejectionId)),
          Some(MessageId(unloadingPermissionId)),
          Some(MessageId(unloadingRemarksId))
        )
      ) mustBe Json.obj(
        "arrivalId" -> arrival.arrivalId,
        "messages" ->
          Json.obj(
            "IE007" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$messageId",
            "IE008" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$rejectionId",
            "IE043" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$unloadingPermissionId",
            "IE044" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$unloadingRemarksId"
          )
      )

    }

    "return unloading remarks rejection link" in {

      val messageId                   = 1
      val rejectionId                 = 2
      val unloadingPermissionId       = 3
      val unloadingRemarksId          = 4
      val unloadingRemarksRejectionId = 5

      Json.toJson(
        MessagesSummary(
          arrival,
          MessageId(messageId),
          Some(MessageId(rejectionId)),
          Some(MessageId(unloadingPermissionId)),
          Some(MessageId(unloadingRemarksId)),
          Some(MessageId(unloadingRemarksRejectionId))
        )
      ) mustBe Json.obj(
        "arrivalId" -> arrival.arrivalId,
        "messages" ->
          Json.obj(
            "IE007" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$messageId",
            "IE008" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$rejectionId",
            "IE043" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$unloadingPermissionId",
            "IE044" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$unloadingRemarksId",
            "IE058" -> s"/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages/$unloadingRemarksRejectionId"
          )
      )

    }

  }

}
