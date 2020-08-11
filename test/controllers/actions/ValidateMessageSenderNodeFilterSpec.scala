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
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.ActionFilter
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.twirl.api.HtmlFormat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.NodeSeq

class ValidateMessageSenderNodeFilterSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ScalaFutures with GuiceOneAppPerTest {

  def action: ValidateMessageSenderNodeFilter = new ValidateMessageSenderNodeFilter()

  def fakeRequest(node: NodeSeq): FakeRequest[NodeSeq] = FakeRequest().withBody(node)

  def fakeRequestNoContent: FakeRequest[AnyContent] = FakeRequest()

  def successfulRequest[A]: Request[A] => Future[Result] = {
    _ =>
      Future.successful(Ok(HtmlFormat.empty))
  }

  "ValidateMessageSenderNodeFilter" - {

    "should return BAD_REQUEST if no content exists" in {

      val testAction: ActionFilter[Request] = action.filter

      val result = testAction.invokeBlock(fakeRequestNoContent, successfulRequest)
      status(result) mustBe BAD_REQUEST
    }

    "should return BAD_REQUEST if MesSenMES3 node exists" in {

      val testAction: ActionFilter[Request] = action.filter

      val result = testAction.invokeBlock(fakeRequest(<MesSenMES3></MesSenMES3>), successfulRequest)
      status(result) mustBe BAD_REQUEST
    }

    "execute block if MesSenMES3 does not exist" in {

      val testAction: ActionFilter[Request] = action.filter

      val result = testAction.invokeBlock(fakeRequest(<test></test>), successfulRequest)
      status(result) mustBe OK
    }

  }
}
