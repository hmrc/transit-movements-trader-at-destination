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

import generators.ModelGenerators
import models.Arrival
import models.ChannelType.web
import models.MovementReferenceNumber
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.OptionValues
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsString
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.Headers
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.Future
import scala.xml.NodeSeq

class AuthenticatedGetOptionalArrivalForWriteActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[NodeSeq] = FakeRequest("", "").withBody(<CC007A>
      <HEAHEA>
        <DocNumHEA5>{MovementReferenceNumber("MRN").value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>)

  class Harness(authOptionalGet: AuthenticatedGetOptionalArrivalForWriteActionProvider, cc: ControllerComponents) extends BackendController(cc) {

    def get: Action[NodeSeq] = authOptionalGet()(parse.xml) {
      request =>
        Results.Ok(JsBoolean(request.arrival.isDefined))
    }

    def getJson: Action[AnyContent] = authOptionalGet() {
      request =>
        Results.Ok(JsBoolean(request.arrival.isDefined))
    }

    def failingAction: Action[NodeSeq] = authOptionalGet()(cc.parsers.xml).async {
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
        when(mockArrivalMovementRepository.get(any(), any(), any())) thenReturn Future.successful(None)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)

          val result = controller.get()(fakeRequest.withHeaders("channel" -> web.toString))
          status(result) mustBe OK
          contentAsJson(result) mustBe JsBoolean(false)
        }
      }

      "must lock, get and unlock an arrival when it exists and its EORI matches the user's" in {

        val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any(), any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)
          val result     = controller.get()(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

          status(result) mustBe OK
          contentAsJson(result) mustBe JsBoolean(true)
          verify(mockLockRepository, times(1)).lock(eqTo(arrival.arrivalId))
          verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
        }
      }

      "must unlock the arrival if we find and lock one and return Internal Server Error if the main action fails" in {

        val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any(), any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)
          val result     = controller.failingAction()(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

          status(result) mustBe INTERNAL_SERVER_ERROR
          verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
        }
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

      "must return Forbidden" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)
          val result     = controller.get(fakeRequest.withHeaders("channel" -> web.toString))

          status(result) mustBe FORBIDDEN
        }
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
        when(mockArrivalMovementRepository.get(any(), any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)
          val result     = controller.get()(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

          status(result) mustBe LOCKED
        }
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
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)

          val result = controller.get()(FakeRequest().withBody(<CC007A>
            <HEAHEA>
            </HEAHEA>
          </CC007A>))

          status(result) mustBe BAD_REQUEST
        }
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
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)

          val request: FakeRequest[AnyContent] = FakeRequest().withBody(AnyContent(JsString("Happy Apples")))

          val result = controller.getJson()(request)

          status(result) mustBe BAD_REQUEST
        }
      }
    }

    "when channel header missing" - {
      "must return BadRequest" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)

          val result = controller.get()(fakeRequest.withHeaders(Headers.create()))

          status(result) mustBe BAD_REQUEST
        }
      }
    }

    "when channel header contains invalid value" - {
      "must return BadRequest" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetOptionalArrivalForWriteActionProvider]
          val cc             = application.injector.instanceOf[ControllerComponents]

          val controller = new Harness(actionProvider, cc)

          val result = controller.get()(fakeRequest.withHeaders("channel" -> "web2"))

          status(result) mustBe BAD_REQUEST
        }
      }
    }
  }
}
