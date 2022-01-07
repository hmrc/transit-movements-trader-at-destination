/*
 * Copyright 2022 HM Revenue & Customs
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

import audit.AuditService
import audit.AuditType
import audit.AuthenticationDetails
import migrations.FakeMigrationRunner
import migrations.MigrationRunner
import models.ChannelType
import models.ChannelType.web
import org.mockito.Mockito._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.Headers
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.scalatest.BeforeAndAfterEach

import scala.concurrent.Future

class AuthenticateActionProviderSpec extends AnyFreeSpec with Matchers with MockitoSugar with BeforeAndAfterEach {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "").withHeaders("channel" -> web.toString())

  class Harness(authenticate: AuthenticateActionProvider) {

    def action(): Action[AnyContent] = authenticate() {
      result =>
        Results.Ok(result.enrolmentId.fold(_.value, _.value, (_, eori) => eori.value))
    }
  }

  val eoriNumber = "EORI"
  val turn       = "TURN"

  val unrelatedEnrolment =
    Enrolment(
      key = "IR-SA",
      identifiers = Seq(
        EnrolmentIdentifier(
          "UTR",
          "123"
        )
      ),
      state = "Activated"
    )

  val newEnrolment =
    Enrolment(
      key = "HMRC-CTC-ORG",
      identifiers = Seq(
        EnrolmentIdentifier(
          "EORINumber",
          eoriNumber
        )
      ),
      state = "Activated"
    )

  val legacyEnrolment =
    Enrolment(
      key = "HMCE-NCTS-ORG",
      identifiers = Seq(
        EnrolmentIdentifier(
          "VATRegNoTURN",
          turn
        )
      ),
      state = "Activated"
    )

  val mockAuditService = mock[AuditService]

  override def beforeEach(): Unit =
    reset(mockAuditService)

  "authenticate" - {

    "when a user has valid enrolments" - {

      val validNewEnrolments: Enrolments    = Enrolments(Set(unrelatedEnrolment, newEnrolment))
      val validLegacyEnrolments: Enrolments = Enrolments(Set(unrelatedEnrolment, legacyEnrolment))
      val validBothEnrolments: Enrolments   = Enrolments(Set(unrelatedEnrolment, newEnrolment, legacyEnrolment))

      "must pass on the user's EORI" in {

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validNewEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to(classOf[FakeMigrationRunner]),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.action()(fakeRequest)

          status(result) mustEqual OK
          contentAsString(result) mustEqual eoriNumber
          verify(mockAuditService, times(1)).authAudit(eqTo(AuditType.SuccessfulAuthTracking), eqTo(AuthenticationDetails(ChannelType.web, "Modern")))(any())
        }
      }

      "must pass on the user's TURN" in {

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validLegacyEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to(classOf[FakeMigrationRunner]),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.action()(fakeRequest)

          status(result) mustEqual OK
          contentAsString(result) mustEqual turn
          verify(mockAuditService, times(1)).authAudit(eqTo(AuditType.SuccessfulAuthTracking), eqTo(AuthenticationDetails(ChannelType.web, "Legacy")))(any())
        }
      }

      "must pick user's EORI over their TURN if both enrolments are present" in {

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validBothEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to(classOf[FakeMigrationRunner]),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.action()(fakeRequest)

          status(result) mustEqual OK
          contentAsString(result) mustEqual eoriNumber
          verify(mockAuditService, times(1)).authAudit(eqTo(AuditType.SuccessfulAuthTracking), eqTo(AuthenticationDetails(ChannelType.web, "Modern")))(any())
        }
      }
    }

    "when channel header is invalid" - {

      val eoriNumber = "EORI"

      val validEnrolments: Enrolments = Enrolments(
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
                eoriNumber
              )
            ),
            state = "Activated"
          )
        )
      )

      "must return BadRequest" in {

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to(classOf[FakeMigrationRunner])
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.action()(fakeRequest.withHeaders("channel" -> "web2"))

          status(result) mustEqual BAD_REQUEST
          contentAsString(result) mustEqual "Missing channel header or incorrect value specified in channel header"
        }
      }
    }

    "when channel header is missing" - {

      val eoriNumber = "EORI"

      val validEnrolments: Enrolments = Enrolments(
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
                eoriNumber
              )
            ),
            state = "Activated"
          )
        )
      )

      "must return BadRequest" in {

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to(classOf[FakeMigrationRunner])
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.action()(fakeRequest.withHeaders(Headers.create()))

          status(result) mustEqual BAD_REQUEST
          contentAsString(result) mustEqual "Missing channel header or incorrect value specified in channel header"
        }
      }
    }

    "when a user has invalid enrolments" - {

      val invalidEnrolments = Enrolments(
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
          )
        )
      )

      "must return Forbidden" in {

        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to(classOf[FakeMigrationRunner])
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.action()(fakeRequest)

          status(result) mustBe FORBIDDEN
        }
      }
    }
  }
}
