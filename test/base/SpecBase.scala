/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.LocalDate

import models.TraderWithEori
import models.messages.NormalNotification
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.HeaderCarrier

trait SpecBase
  extends FreeSpec
    with GuiceOneAppPerSuite
    with MustMatchers
    with MockitoSugar
    with ScalaFutures
    with OptionValues {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  protected def applicationBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()

  lazy val normalNotification = NormalNotification(
    movementReferenceNumber = "movementReferenceNumber",
    notificationPlace = "notificationPlace",
    notificationDate = LocalDate.now(),
    customsSubPlace = None,
    trader = TraderWithEori("eori", None, None, None, None, None),
    presentationOffice = "sadsf",
    enRouteEvents = Seq.empty
  )

}
