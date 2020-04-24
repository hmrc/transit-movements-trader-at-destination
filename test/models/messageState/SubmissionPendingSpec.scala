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

import generators.ModelGenerators
import models.MessageState.SubmissionFailed
import models.MessageState.SubmissionPending
import models.MessageState.SubmissionSucceeded
import models.SubmissionResult
import models.SubmissionResult.Failure
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SubmissionPendingSpec extends FreeSpec with MustMatchers with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "SubmissionPending must transition" - {

    "to SubmittedSucceeded when receiving a SubmittedSucceeded event" in {
      SubmissionPending.transition(SubmissionResult.Success) mustEqual SubmissionSucceeded
    }

    "to SubmissionFailed when receiving a SubmissionFailed event" in {
      forAll(arbitrary[Failure]) {
        failure =>
          SubmissionPending.transition(failure) mustEqual SubmissionFailed
      }

    }
  }
}
