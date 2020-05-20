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

package models

import base.SpecBase
import generators.ModelGenerators
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

class ArrivalSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  val arrivaGenerator: Gen[Arrival] =
    for {
      messages <- nonEmptyListOfMaxLength[MovementMessageWithStatus](20)
      arrival  <- arbitrary[Arrival].map(_.copy(messages = messages))
    } yield arrival

  "nextMessageId returns a MessageId which has value that is 1 larger than the number of messages" in {
    forAll(arrivaGenerator) {
      arrival =>
        (MessageId.unapply(arrival.nextMessageId).value - arrival.messages.length) mustEqual 1
    }
  }

  "messageWithId returns a list with the message and the MessageId whose value is one more than the index" in {
    forAll(arrivaGenerator) {
      arrival =>
        arrival.messagesWithId.zipWithIndex.toList.foreach {
          case ((message, messageId), index) =>
            message mustEqual arrival.messages.toList(index)
            (MessageId.unapply(messageId).value - index) mustEqual 1
        }
    }
  }

}
