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
import models.ArrivalStatus.ArrivalRejected
import models.ArrivalStatus.ArrivalXMLSubmissionNegativeAcknowledgement
import models.ArrivalStatus.GoodsReleased
import models.ArrivalStatus.UnloadingPermission
import models.ArrivalStatus.UnloadingRemarksRejected
import models.ArrivalStatus.UnloadingRemarksXMLSubmissionNegativeAcknowledgement
import models.Arrival
import models.ArrivalId
import models.ArrivalRejectedResponse
import models.ArrivalStatus
import models.ChannelType
import models.GoodsReleasedResponse
import models.MessageInbound
import models.MessageType
import models.UnloadingPermissionResponse
import models.UnloadingRemarksRejectedResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import models.ChannelType.web
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

  val arrival: Arrival     = arbitrary[Arrival].sample.value
  val arrivalId: ArrivalId = arrival.arrivalId

  def fakeRequest(code: String, arrivalStatus: ArrivalStatus): ArrivalRequest[AnyContentAsEmpty.type] =
    ArrivalRequest(FakeRequest("", "").withHeaders("X-Message-Type" -> code), arrival.copy(status = arrivalStatus), web)

  val successfulResponse: Request[AnyContent] => Future[Result] = {
    _ =>
      Future.successful(Ok(HtmlFormat.empty))
  }

  def action = new InboundMessageTransformer()

  val responseMessages: Map[String, MessageInbound] = Map(
    MessageType.ArrivalRejection.code          -> MessageInbound(ArrivalRejectedResponse, ArrivalRejected),
    MessageType.UnloadingRemarksRejection.code -> MessageInbound(UnloadingRemarksRejectedResponse, UnloadingRemarksRejected),
    MessageType.UnloadingPermission.code       -> MessageInbound(UnloadingPermissionResponse, UnloadingPermission),
    MessageType.GoodsReleased.code             -> MessageInbound(GoodsReleasedResponse, GoodsReleased),
    MessageType.XMLSubmissionNegativeAcknowledgement.code -> MessageInbound(XMLSubmissionNegativeAcknowledgementResponse,
                                                                            ArrivalXMLSubmissionNegativeAcknowledgement)
  )

  "InboundMessageTransformer" - {

    "handle unsupported `X-Message-Type`" in {

      val testAction = action.invokeBlock(fakeRequest("invalid-code", GoodsReleased), successfulResponse)

      testAction.futureValue.header.status mustBe BAD_REQUEST
    }

    "return correct http response for message type" in {

      forAll(Gen.oneOf(MessageType.values)) {
        message =>
          if (responseMessages.exists(x => message.code == x._1)) {

            val messageInbound: MessageInbound = responseMessages(message.code)

            val testAction = action.invokeBlock(fakeRequest(message.code, messageInbound.nextState), successfulResponse)

            testAction.futureValue.header.status mustBe OK

          } else {
            val testAction = action.invokeBlock(fakeRequest(message.code, GoodsReleased), successfulResponse)

            testAction.futureValue.header.status mustBe BAD_REQUEST
          }
      }

    }

    "return correct MessageResponse for incoming message `X-Message-Type`" in {

      forAll(Gen.oneOf(MessageType.values), Gen.oneOf(ChannelType.values)) {
        (message, channel) =>
          if (responseMessages.exists(x => message.code == x._1)) {
            action.messageResponse(Some(message.code), channel).value mustBe responseMessages(message.code).messageType
          } else {
            action.messageResponse(Some(message.code), channel) mustBe None
          }
      }
    }

    "handle out-of-sequence-message" in {
      val testAction = action.invokeBlock(fakeRequest("IE044", ArrivalStatus.UnloadingRemarksSubmitted), successfulResponse)

      testAction.futureValue.header.status mustBe BAD_REQUEST
    }

  }

}
