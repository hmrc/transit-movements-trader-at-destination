/*
 * Copyright 2023 HM Revenue & Customs
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
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArrivalSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  val arrivalGenerator: Gen[Arrival] =
    for {
      messages <- nonEmptyListOfMaxLength[MovementMessageWithStatus](20)
      arrival  <- arbitrary[Arrival].map(_.copy(messages = messages))
    } yield arrival

  "nextMessageId returns a MessageId which has value that is 1 larger than the number of messages" in {
    forAll(arrivalGenerator) {
      arrival =>
        (MessageId.unapply(arrival.nextMessageId).value - arrival.messages.length) mustEqual 1
    }
  }

  "messageWithId returns a list with the message and its corresponding message ID" in {
    forAll(arrivalGenerator) {
      arrival =>
        arrival.messagesWithId.zipWithIndex.toList.foreach {
          case ((message, messageId), index) =>
            message mustEqual arrival.messages.toList(index)
            messageId mustEqual message.messageId
        }
    }
  }

}
