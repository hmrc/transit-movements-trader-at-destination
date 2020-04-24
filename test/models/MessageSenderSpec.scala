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

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.EitherValues
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.PathBindable

class MessageSenderSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with OptionValues with EitherValues {

  "Message Sender" - {

    "must build from a valid string" in {

      val validString = "MDTP-123-1"

      MessageSender(validString).value mustEqual MessageSender(ArrivalId(123), 1)
    }

    "must not build from an invalid string" in {

      val pattern = "MDTP-(\\d+)-(\\d+)".r.anchored

      forAll(arbitrary[String]) {
        value =>
          whenever(pattern.findFirstMatchIn(value).isEmpty) {
            MessageSender(value) must not be defined
          }
      }
    }

    "must convert to string in the correct format" in {

      val messageSender = MessageSender(ArrivalId(123), 1)

      messageSender.toString mustEqual "MDTP-123-1"
    }

    val pathBindable = implicitly[PathBindable[MessageSender]]

    "must bind from a URL" in {

      val messageSender = MessageSender(ArrivalId(123), 1)

      pathBindable.bind("key", messageSender.toString).right.value mustEqual messageSender
    }

    "must unbind from a URL" in {

      val messageSender = MessageSender(ArrivalId(123), 1)

      pathBindable.unbind("key", messageSender) mustEqual messageSender.toString
    }
  }
}
