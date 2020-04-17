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

import generators.MessageGenerators
import models.Arrival
import models.ArrivalId
import models.MovementReferenceNumber
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsString
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.AnyContentAsXml
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments
import utils.Format

import scala.concurrent.Future

class AuthenticatedGetOptionalArrivalForWriteActionProviderSpec
    extends FreeSpec
    with MustMatchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsXml] = FakeRequest("", "").withXmlBody(<CC007A>
      <HEAHEA>
        <DocNumHEA5>{MovementReferenceNumber("MRN").value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>)

  class Harness(authOptionalGet: AuthenticatedGetOptionalArrivalForWriteActionProvider) {

    def get: Action[AnyContent] = authOptionalGet() {
      request =>
        Results.Ok(JsBoolean(request.arrival.isDefined))
    }

    def failingAction: Action[AnyContent] = authOptionalGet().async {
      _ =>
        Future.failed(new Exception)
    }
  }

  val eoriNumber = "123"

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

  "authenticated get arrival for write" - {

    "when given valid enrolments" - {

      "must not lock and does not return an arrival when one does not exist" in {

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any(), any())) thenReturn Future.successful(None)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)

        val result = controller.get()(fakeRequest)
        status(result) mustBe OK
        contentAsJson(result) mustBe JsBoolean(false)
      }

      "must lock, get and unlock an arrival when it exists and its EORI matches the user's" in {

        val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get()(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe JsBoolean(true)
        verify(mockLockRepository, times(1)).lock(eqTo(arrival.arrivalId))
        verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
      }

      "must unlock the arrival if we find and lock one and return Internal Server Error if the main action fails" in {

        val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.failingAction()(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
      }
    }

    "when given invalid enrolments" - {

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

      "must return Unauthorized" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(fakeRequest)

        status(result) mustBe UNAUTHORIZED
      }
    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {
        val arrival = arbitrary[Arrival].sample.value

        val mockLockRepository               = mock[LockRepository]
        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get()(fakeRequest)

        status(result) mustBe LOCKED
      }
    }

    "when we can't parse an mrn from the body" - {
      "must return BadRequest" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)

        val result = controller.get()(FakeRequest().withXmlBody(<CC007A>
          <HEAHEA>
          </HEAHEA>
        </CC007A>))
        status(result) mustBe BAD_REQUEST
      }
    }

    "when have something other than xml in the body" - {
      "must return BadRequest" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]

        val controller = new Harness(actionProvider)

        val result = controller.get()(FakeRequest().withJsonBody(JsString("Happy Apples")))
        status(result) mustBe BAD_REQUEST
      }
    }
  }
}
