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

package controllers.actions

import cats.data.Ior
import generators.ModelGenerators
import models._
import models.request.ArrivalWithoutMessagesRequest
import models.request.AuthenticatedRequest
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.TryValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import play.api.mvc.AnyContentAsEmpty
import play.api.test.DefaultAwaitTimeout
import play.api.test.FakeRequest

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class ValidateOutboundMessageActionSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ModelGenerators
    with OptionValues
    with ScalaFutures
    with TryValues
    with EitherValues
    with DefaultAwaitTimeout {

  private val arrivalWithoutMessages: ArrivalWithoutMessages = arbitrary[ArrivalWithoutMessages].sample.value
  val actionRefiner: ValidateOutboundMessageAction           = new ValidateOutboundMessageAction()
  val fakeRequest: FakeRequest[AnyContentAsEmpty.type]       = FakeRequest("", "")

  val fakeArrivalWithoutMessagesRequest: ArrivalWithoutMessagesRequest[AnyContentAsEmpty.type] =
    ArrivalWithoutMessagesRequest(
      AuthenticatedRequest(fakeRequest, ChannelType.web, Ior.right(EORINumber("eori"))),
      arrivalWithoutMessages,
      ChannelType.web
    )

  "refine" - {
    Seq(
      GoodsReleasedResponse                        -> ArrivalStatus.GoodsReleased,
      ArrivalRejectedResponse                      -> ArrivalStatus.ArrivalRejected,
      UnloadingPermissionResponse                  -> ArrivalStatus.UnloadingPermission,
      UnloadingRemarksRejectedResponse             -> ArrivalStatus.UnloadingRemarksRejected,
      XMLSubmissionNegativeAcknowledgementResponse -> ArrivalStatus.ArrivalXMLSubmissionNegativeAcknowledgement
    ) foreach {
      case (response, st) =>
        s"return an internal Server if an Inbound message ($response) is found" in {

          import play.api.test.Helpers.BAD_REQUEST
          import play.api.test.Helpers.status

          status(
            actionRefiner
              .refine(MessageTransformRequest(Message(response, st), fakeArrivalWithoutMessagesRequest))
              .map(_.left.value)
          ) mustBe BAD_REQUEST
        }
    }
    Seq(
      UnloadingRemarksResponse -> ArrivalStatus.UnloadingRemarksSubmitted
    ) foreach {
      case (response, status) =>
        s"return internal server error if an outbound response ($response) is found" in {

          actionRefiner
            .refine(MessageTransformRequest(Message(response, status), fakeArrivalWithoutMessagesRequest))
            .value mustBe Some(Success(Right(OutboundMessageRequest(OutboundMessage(response, status), fakeArrivalWithoutMessagesRequest))))
        }
    }
  }

}
