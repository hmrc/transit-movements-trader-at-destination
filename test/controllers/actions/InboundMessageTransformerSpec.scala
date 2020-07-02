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

package controllers.actions
import generators.ModelGenerators
import models.ArrivalStatus.ArrivalRejected
import models.ArrivalStatus.GoodsReleased
import models.ArrivalStatus.UnloadingPermission
import models.ArrivalStatus.UnloadingRemarksRejected
import models.Arrival
import models.ArrivalRejectedResponse
import models.ArrivalStatus
import models.GoodsReleasedResponse
import models.MessageInbound
import models.UnloadingPermissionResponse
import models.UnloadingRemarksRejectedResponse
import models.request.ArrivalRequest
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc._
import play.api.test.FakeRequest

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class InboundMessageTransformerSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators with OptionValues with ScalaFutures {

  val arrival: Arrival = arbitrary[Arrival].sample.value
  val arrivalId        = arrival.arrivalId

  def fakeRequest(code: String, arrivalStatus: ArrivalStatus): ArrivalRequest[AnyContentAsEmpty.type] =
    ArrivalRequest(FakeRequest("", "").withHeaders("X-Message-Type" -> code), arrival.copy(status = arrivalStatus))

  def action() = new InboundMessageTransformer()

  "InboundMessageTransformer" - {

    "handle unsupported X-Message-Type" in {

      val testAction: Future[InboundRequest[_]] = action().transform(fakeRequest("invalid-code", GoodsReleased))

      testAction.futureValue.inboundMessage mustBe None
    }

    "transform the request when X-Message-Type is" - {

      "IE025" in {
        val testAction: Future[InboundRequest[_]] = action().transform(fakeRequest("IE025", GoodsReleased))

        testAction.futureValue.inboundMessage mustBe
          Some(MessageInbound(GoodsReleasedResponse, GoodsReleased))
      }

      "IE043" in {
        val testAction: Future[InboundRequest[_]] = action().transform(fakeRequest("IE043", UnloadingPermission))

        testAction.futureValue.inboundMessage mustBe
          Some(MessageInbound(UnloadingPermissionResponse, UnloadingPermission))
      }

      "IE008" in {
        val testAction: Future[InboundRequest[_]] = action().transform(fakeRequest("IE008", ArrivalRejected))

        testAction.futureValue.inboundMessage mustBe
          Some(MessageInbound(ArrivalRejectedResponse, ArrivalRejected))
      }

      "IE058" in {
        val testAction: Future[InboundRequest[_]] = action().transform(fakeRequest("IE058", UnloadingRemarksRejected))

        testAction.futureValue.inboundMessage mustBe
          Some(MessageInbound(UnloadingRemarksRejectedResponse, UnloadingRemarksRejected))
      }
    }

  }

}
