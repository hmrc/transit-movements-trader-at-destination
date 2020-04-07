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
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments

import scala.concurrent.Future

class AuthenticatedGetArrivalForReadActionProviderSpec
    extends FreeSpec
    with MustMatchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(authAndGet: AuthenticatedGetArrivalForReadActionProvider) {

    def get(arrivalId: ArrivalId): Action[AnyContent] = authAndGet(arrivalId) {
      request =>
        Results.Ok(request.arrival.arrivalId.toString)
    }
  }

  "authenticated get arrival for read" - {

    "when given valid enrolments" - {

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

      "must get an arrival when it exists and its EORI matches the user's" in {

        val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any())) thenReturn Future.successful(Some(arrival))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetArrivalForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(arrival.arrivalId)(fakeRequest)

        status(result) mustBe OK
        contentAsString(result) mustBe arrival.arrivalId.toString
      }

      "must return Not Found when the arrival exists and its EORI does not match the user's" in {

        val arrival = arbitrary[Arrival].sample.value copy (eoriNumber = "invalid EORI number")

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any())) thenReturn Future.successful(Some(arrival))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetArrivalForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(arrival.arrivalId)(fakeRequest)

        status(result) mustBe NOT_FOUND
      }

      "must return Not Found when the arrival does not exist" in {

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.get(any())) thenReturn Future.successful(None)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetArrivalForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(arrivalId)(fakeRequest)

        status(result) mustBe NOT_FOUND
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

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetArrivalForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(arrivalId)(fakeRequest)

        status(result) mustBe UNAUTHORIZED
      }
    }
  }
}
