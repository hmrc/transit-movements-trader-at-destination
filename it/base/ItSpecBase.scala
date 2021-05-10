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

package base

import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageStatus.SubmissionPending
import models.Arrival
import models.MovementMessageWithStatus
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.TryValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.inject.guice.GuiceApplicationBuilder

class ItSpecBase
    extends AnyFreeSpec
    with Matchers
    with ScalaFutures
    with IntegrationPatience
    with OptionValues
    with EitherValues
    with TryValues
    with ModelGenerators {

  val arrivalWithOneMessage: Gen[Arrival] = for {
    arrival         <- arbitrary[Arrival]
    movementMessage <- arbitrary[MovementMessageWithStatus]
  } yield arrival.copy(messages = NonEmptyList.one(movementMessage.copy(status = SubmissionPending)))

}
