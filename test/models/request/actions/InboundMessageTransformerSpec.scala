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

package models.request.actions

import generators.ModelGenerators
import models.ArrivalStatus.ArrivalSubmitted
import models.ArrivalStatus.ArrivalXMLSubmissionNegativeAcknowledgement
import models.ArrivalStatus.UnloadingPermission
import models.ArrivalStatus.UnloadingRemarksXMLSubmissionNegativeAcknowledgement
import models.Arrival
import models.ArrivalId
import models.ArrivalRejectedResponse
import models.ArrivalStatus
import models.ChannelType
import models.GoodsReleasedResponse
import models.MessageType
import models.UnloadingPermissionResponse
import models.UnloadingRemarksRejectedResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import models.request.ArrivalRequest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.Results.Ok
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.twirl.api.HtmlFormat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class InboundMessageTransformerSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators with OptionValues with ScalaFutures {

  private val arrival: Arrival     = arbitrary[Arrival].sample.value
  private val arrivalId: ArrivalId = arrival.arrivalId

  private val successfulResponse: Request[AnyContent] => Future[Result] = {
    _ =>
      Future.successful(Ok(HtmlFormat.empty))
  }

  private def action = new InboundMessageTransformer()

  private val xmlNegativeAcknowledgement: ArrivalStatus =
    Gen.oneOf(Seq(ArrivalXMLSubmissionNegativeAcknowledgement, UnloadingRemarksXMLSubmissionNegativeAcknowledgement)).sample.value

  "InboundMessageTransformer" - {

    "returns 200 for supported `X-Message-Type`" in {
      forAll(arbitrary[ChannelType]) {
        channel =>
          val arrivalMovement = arrival.copy(status = ArrivalStatus.ArrivalSubmitted)

          val request =
            ArrivalRequest(FakeRequest("", "").withHeaders("X-Message-Type" -> MessageType.ArrivalRejection.code), arrivalMovement, channel)

          val testAction = action.invokeBlock(request, successfulResponse)

          testAction.futureValue.header.status mustBe OK

      }
    }

    "return 400 for unsupported `X-Message-Type`" in {
      forAll(arbitrary[ChannelType]) {
        channel =>
          val request =
            ArrivalRequest(FakeRequest("", "").withHeaders("X-Message-Type" -> "invalid-message-type"), arrival, channel)

          val testAction = action.invokeBlock(request, successfulResponse)

          testAction.futureValue.header.status mustBe BAD_REQUEST

      }
    }

    "when the incoming message is an allowable transition from the current status" in {
      forAll(arbitrary[ChannelType]) {
        channel =>
          val arrivalMovement = arrival.copy(status = ArrivalSubmitted)

          val request =
            ArrivalRequest(FakeRequest("", "").withHeaders("X-Message-Type" -> MessageType.GoodsReleased.code), arrivalMovement, channel)

          val testAction = action.invokeBlock(request, successfulResponse)

          testAction.futureValue.header.status mustBe OK

      }

    }

    "when the incoming message is not an allowable transition from the current status" in {
      forAll(arbitrary[ChannelType]) {
        channel =>
          val arrivalMovement = arrival.copy(status = UnloadingPermission)

          val request =
            ArrivalRequest(FakeRequest("", "").withHeaders("X-Message-Type" -> MessageType.ArrivalRejection.code), arrivalMovement, channel)

          val testAction = action.invokeBlock(request, successfulResponse)

          testAction.futureValue.header.status mustBe BAD_REQUEST

      }
    }

    ".messageResponse" - {
      "determines the MessageResponse for the `X-Message-Type` header" in {

        //TODO: This test needs to be refactored. This is a duplication of logic from the implementation.
        val messageTypeAndExpectedMessageResponse = Seq(
          (MessageType.GoodsReleased, GoodsReleasedResponse),
          (MessageType.ArrivalRejection, ArrivalRejectedResponse),
          (MessageType.UnloadingPermission, UnloadingPermissionResponse),
          (MessageType.UnloadingRemarksRejection, UnloadingRemarksRejectedResponse),
          (MessageType.XMLSubmissionNegativeAcknowledgement, XMLSubmissionNegativeAcknowledgementResponse)
        )

        forAll(arbitrary[ChannelType]) {
          channel =>
            messageTypeAndExpectedMessageResponse.foreach {
              case (messageType, messageResponse) =>
                action.messageResponse(channel)(messageType.code).value mustEqual messageResponse
                ()
            }
        }
      }
    }
  }

}
