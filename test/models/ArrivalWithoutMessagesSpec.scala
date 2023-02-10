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

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageType.ArrivalNotification
import models.MessageType.GoodsReleased
import models.MessageType.UnloadingPermission
import models.MessageType.UnloadingRemarks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import play.api.libs.json.Json
import org.scalacheck.Arbitrary.arbitrary
import java.time.temporal.ChronoUnit
import java.time.LocalDateTime

class ArrivalWithoutMessagesSpec extends SpecBase with MongoDateTimeFormats with ModelGenerators {

  val expectedDateTime             = LocalDateTime.now.truncatedTo(ChronoUnit.MILLIS)
  val expectedDateTimeMinusHours   = LocalDateTime.now.minusHours(1).truncatedTo(ChronoUnit.MILLIS)
  val expectedDateTimeMinusMinutes = LocalDateTime.now.minusMinutes(30).truncatedTo(ChronoUnit.MILLIS)
  val expectedDateDays             = LocalDateTime.now.minusDays(2).truncatedTo(ChronoUnit.MILLIS)

  val message1 = MovementMessageWithoutStatus(MessageId(1), expectedDateDays, Some(expectedDateDays), UnloadingPermission, <foo></foo>, 1)
  val message2 = MovementMessageWithoutStatus(MessageId(2), expectedDateTimeMinusMinutes, Some(expectedDateTimeMinusMinutes), GoodsReleased, <foo></foo>, 2)
  val message3 = MovementMessageWithoutStatus(MessageId(3), expectedDateTimeMinusHours, Some(expectedDateTimeMinusHours), UnloadingRemarks, <foo></foo>, 3)
  val message4 = MovementMessageWithoutStatus(MessageId(4), expectedDateTime, Some(expectedDateTime), ArrivalNotification, <foo></foo>, 4)

  val messages: Seq[MovementMessageWithoutStatus] = Seq(message1, message2, message3, message4)

  "ArrivalWithoutMessages" - {
    "must serialise with list of messageMetaData" in {

      val movementMessageInJson = Json.obj(
        "_id"                      -> ArrivalId(1),
        "channel"                  -> "web",
        "movementReferenceNumber"  -> MovementReferenceNumber("mrn"),
        "eoriNumber"               -> "1234567",
        "status"                   -> "Initialized",
        "created"                  -> LocalDateTime.now(),
        "updated"                  -> LocalDateTime.now(),
        "lastUpdated"              -> LocalDateTime.now(),
        "nextMessageId"            -> MessageId(1),
        "nextMessageCorrelationId" -> 1,
        "messages"                 -> messages
      )

      val expectedResult = Seq(
        MessageMetaData(message1.messageType, message1.dateTime),
        MessageMetaData(message2.messageType, message2.dateTime),
        MessageMetaData(message3.messageType, message3.dateTime),
        MessageMetaData(message4.messageType, message4.dateTime)
      )

      val result = movementMessageInJson.validate[ArrivalWithoutMessages].asOpt.value

      result.messagesMetaData mustBe expectedResult
    }

    "must create from Arrival" in {

      forAll(arbitrary[Arrival]) {
        arrival =>
          val messageList = NonEmptyList(message1, List(message2, message3, message4))

          val updatedArrival = arrival.copy(messages = messageList)

          val expectedResult =
            ArrivalWithoutMessages(
              updatedArrival.arrivalId,
              updatedArrival.channel,
              updatedArrival.movementReferenceNumber,
              updatedArrival.eoriNumber,
              updatedArrival.created,
              updatedArrival.updated,
              updatedArrival.lastUpdated,
              updatedArrival.notificationBox,
              updatedArrival.nextMessageId,
              updatedArrival.nextMessageCorrelationId,
              Seq(
                MessageMetaData(message1.messageType, message1.dateTime),
                MessageMetaData(message2.messageType, message2.dateTime),
                MessageMetaData(message3.messageType, message3.dateTime),
                MessageMetaData(message4.messageType, message4.dateTime)
              )
            )

          ArrivalWithoutMessages.fromArrival(updatedArrival) mustBe expectedResult
      }
    }
  }
}
