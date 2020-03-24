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

import models.State._
import models.MessageReceived
import models.SubmissionResult
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers

class PendingSubmissionSpec extends FreeSpec with MustMatchers {

  "Pending Submission must transition" - {

    "to Submitted when receiving a Submission Success event" in {
      PendingSubmission.transition(SubmissionResult.Success) mustEqual Submitted
    }

    "to Submission Failed when receiving a Submission Failure event" in {
      PendingSubmission.transition(SubmissionResult.Failure) mustEqual SubmissionFailed
    }

    "to Goods Released when receiving a Goods Released event" in {
      PendingSubmission.transition(MessageReceived.GoodsReleased) mustEqual GoodsReleased
    }
  }
}
