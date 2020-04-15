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

package models.state

import models.ArrivalState._
import models.MessageReceived
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers

class ArrivalStateSpec extends FreeSpec with MustMatchers {

  "Initialized must transition" - {

    "to Submitted when receiving a ArrivalSubmitted event" in {
      Initialized.transition(MessageReceived.ArrivalSubmitted) mustEqual ArrivalSubmitted
    }

    "to Goods Released when receiving a GoodsReleased event" in {
      Initialized.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }
  }

  "Submitted must transition" - {

    "to Submitted when receiving an ArrivalSubmitted event" in {
      ArrivalSubmitted.transition(MessageReceived.ArrivalSubmitted) mustEqual ArrivalSubmitted
    }

    "to GoodsReceived when receiving a Goods Received event" in {
      ArrivalSubmitted.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }
  }
}
