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

package models.messageState

import models.MessageState.SubmissionFailed
import models.MessageState.SubmissionSucceeded
import models.SubmissionResult
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers

class SubmissionFailedSpec extends FreeSpec with MustMatchers {

  "SubmissionFailed must transition" - {

    "to Submitted when receiving a Submission Success event" in {
      SubmissionFailed.transition(SubmissionResult.Success) mustEqual SubmissionSucceeded
    }

    "to Submission Failed when receiving a Submission Failure event" in {
      SubmissionFailed.transition(SubmissionResult.Failure) mustEqual SubmissionFailed
    }
  }
}
