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
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageType.ArrivalNotification
import models.MessageType.ArrivalRejection
import models.MessageType.UnloadingPermission
import models.MessageType.UnloadingRemarks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import java.time.LocalDateTime

class ArrivalSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  val arrivaGenerator: Gen[Arrival] =
    for {
      messages <- nonEmptyListOfMaxLength[MovementMessageWithStatus](20)
      arrival  <- arbitrary[Arrival].map(_.copy(messages = messages))
    } yield arrival

  "must return latest message" in {
    val message = arbitrary[MovementMessageWithoutStatus].sample.value

    val expectedMessage = message
      .copy(dateTime = LocalDateTime.now)
      .copy(messageType = ArrivalNotification)
    val message1 = message
      .copy(dateTime = LocalDateTime.now.minusMinutes(10))
      .copy(messageType = ArrivalRejection)

    val message2 = message
      .copy(dateTime = LocalDateTime.now.minusHours(5))
      .copy(messageType = UnloadingPermission)

    val message3 = message
      .copy(dateTime = LocalDateTime.now.minusDays(2))
      .copy(messageType = UnloadingRemarks)

    val arrivalWithDateTime = NonEmptyList(message1, List(message2, message3, expectedMessage))

    forAll(arrivaGenerator) {
      arrival =>
        arrival.copy(messages = arrivalWithDateTime).latestMessage mustBe ArrivalNotification

    }

  }

  "nextMessageId returns a MessageId which has value that is 1 larger than the number of messages" in {
    forAll(arrivaGenerator) {
      arrival =>
        (MessageId.unapply(arrival.nextMessageId).value - arrival.messages.length) mustEqual 1
    }
  }

  "messageWithId returns a list with the message and its corresponding message ID" in {
    forAll(arrivaGenerator) {
      arrival =>
        arrival.messagesWithId.zipWithIndex.toList.foreach {
          case ((message, messageId), index) =>
            message mustEqual arrival.messages.toList(index)
            messageId mustEqual message.messageId
        }
    }
  }

}
