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

import base.SpecBase
import config.AppConfig
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.BodyParsers
import play.api.mvc.Results
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class IdentifierActionSpec extends SpecBase {

  class Harness(authAction: IdentifierAction) {

    def onPageLoad(): Action[AnyContent] = authAction {
      _ =>
        Results.Ok
    }
  }

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val enrolmentsWithoutEori: Enrolments = Enrolments(
    Set(
      Enrolment(
        key = "IR-SA",
        identifiers = Seq(
          EnrolmentIdentifier(
            "UTR",
            "123"
          )
        ),
        state = "Activated"
      ),
      Enrolment(
        key = "IR-CT",
        identifiers = Seq(
          EnrolmentIdentifier(
            "UTR",
            "456"
          )
        ),
        state = "Activated"
      )
    )
  )

  val enrolmentsWithEori: Enrolments = Enrolments(
    Set(
      Enrolment(
        key = "IR-SA",
        identifiers = Seq(
          EnrolmentIdentifier(
            "UTR",
            "123"
          )
        ),
        state = "Activated"
      ),
      Enrolment(
        key = "HMCE-NCTS-ORG",
        identifiers = Seq(
          EnrolmentIdentifier(
            "VATRegNoTURN",
            "123"
          )
        ),
        state = "NotYetActivated"
      ),
      Enrolment(
        key = "HMCE-NCTS-ORG",
        identifiers = Seq(
          EnrolmentIdentifier(
            "VATRegNoTURN",
            "456"
          )
        ),
        state = "Activated"
      )
    )
  )

  "IdentifierAction" - {

    "must return Ok when given enrolments that contain a valid eori" in {
      when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
        .thenReturn(Future.successful(enrolmentsWithEori))

      val application = applicationBuilder.build()

      val bodyParsers = application.injector.instanceOf[BodyParsers.Default]
      val appConfig   = application.injector.instanceOf[AppConfig]

      val authAction = new AuthenticatedIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
      val controller = new Harness(authAction)
      val result     = controller.onPageLoad()(fakeRequest)

      status(result) mustBe OK

    }

    "must return 401 when given enrolments that do not contain a valid eori" in {
      when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
        .thenReturn(Future.successful(enrolmentsWithoutEori))

      val application = applicationBuilder.build()
      val bodyParsers = application.injector.instanceOf[BodyParsers.Default]
      val appConfig   = application.injector.instanceOf[AppConfig]

      val authAction = new AuthenticatedIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
      val controller = new Harness(authAction)
      val result     = controller.onPageLoad()(fakeRequest)
      status(result) mustBe UNAUTHORIZED

    }
  }

}
