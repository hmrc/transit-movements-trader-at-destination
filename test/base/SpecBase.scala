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

import controllers.actions.AuthenticateActionProvider
import controllers.actions.FakeAuthenticateActionProvider
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import testOnly.utils.ConvertingXmlToJsonConverter
import testOnly.utils.NoJsonXmlToJsonConverter
import uk.gov.hmrc.http.HeaderCarrier
import utils.XmlToJsonConverter

trait SpecBase extends AnyFreeSpec with Matchers with MockitoSugar with ScalaFutures with IntegrationPatience with OptionValues with EitherValues {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  val emptyConverter = new NoJsonXmlToJsonConverter
  val converter      = new ConvertingXmlToJsonConverter

  protected def baseApplicationBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .overrides(
        bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
        bind[XmlToJsonConverter].to[NoJsonXmlToJsonConverter]
      )
      .configure(
        "metrics.jvm" -> false
      )
}
