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

package base

import java.time.LocalDate

import controllers.actions.FakeIdentifierAction
import controllers.actions.IdentifierAction
import models.messages.NormalNotificationMessage
import models.messages.TraderWithEori
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import reactivemongo.api.commands.UpdateWriteResult
import reactivemongo.api.commands.WriteResult
import uk.gov.hmrc.http.HeaderCarrier

trait SpecBase extends FreeSpec with MustMatchers with MockitoSugar with ScalaFutures with OptionValues {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  protected def applicationBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .overrides(
        bind[IdentifierAction].to[FakeIdentifierAction]
      )

  lazy val normalNotification = NormalNotificationMessage(
    movementReferenceNumber = "movementReferenceNumber",
    notificationPlace = "notificationPlace",
    notificationDate = LocalDate.now(),
    customsSubPlace = None,
    trader = TraderWithEori("eori", None, None, None, None, None),
    presentationOffice = "sadsf",
    enRouteEvents = Option(Seq.empty)
  )

  lazy val fakeWriteResult: WriteResult = {
    UpdateWriteResult(
      ok = true,
      n = 1,
      nModified = 1,
      upserted = Seq.empty,
      writeErrors = Seq.empty,
      writeConcernError = None,
      code = None,
      errmsg = None
    )
  }

}
