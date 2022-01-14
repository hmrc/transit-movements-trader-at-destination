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

import generators.ModelGenerators
import migrations.FakeMigrationRunner
import migrations.MigrationRunner
import models.ChannelType.api
import models.ChannelType.web
import models.ArrivalId
import models.ArrivalWithoutMessages
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments

import scala.concurrent.Future
import audit.AuditService
import models.request.AuthenticatedRequest

class AuthenticatedGetArrivalWithoutMessagesForWriteActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(authLockAndGet: AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider) {

    def get(arrivalId: ArrivalId): Action[AnyContent] = authLockAndGet(arrivalId) {
      request =>
        Results.Ok(request.arrivalWithoutMessages.arrivalId.toString)
    }

    def failingAction(arrivalId: ArrivalId): Action[AnyContent] = authLockAndGet(arrivalId).async {
      _ =>
        Future.failed(new Exception)
    }
  }

  val appBuilder = baseApplicationBuilder
    .configure("metrics.jvm" -> false)
    .overrides(bind[MigrationRunner].to(classOf[FakeMigrationRunner]))

  "authenticated get arrivalWithoutMessages for write" - {

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

      "must lock, get and unlock an arrival when it exists and its EORI matches the user's" in {

        val arrival = arbitrary[ArrivalWithoutMessages].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrival.arrivalId)(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

          status(result) mustBe OK
          contentAsString(result) mustBe arrival.arrivalId.toString
          verify(mockLockRepository, times(1)).lock(eqTo(arrival.arrivalId))
          verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
        }
      }

      "must lock and unlock an arrival and return Not Found when its EORI does not match the user's" in {

        val arrival = arbitrary[ArrivalWithoutMessages].sample.value copy (eoriNumber = "invalid EORI number")

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrival.arrivalId)(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

          status(result) mustBe NOT_FOUND
          verify(mockLockRepository, times(1)).lock(eqTo(arrival.arrivalId))
          verify(mockLockRepository, times(1)).unlock(eqTo(arrival.arrivalId))
        }
      }

      "must lock, unlock, audit and return Not Found when the arrival does not exist" in {

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]
        val mockAuditService: AuditService   = mock[AuditService]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(None)
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrivalId)(fakeRequest.withHeaders("channel" -> web.toString))

          status(result) mustBe NOT_FOUND
          verify(mockLockRepository, times(1)).lock(eqTo(arrivalId))
          verify(mockLockRepository, times(1)).unlock(eqTo(arrivalId))
          verify(mockAuditService, times(1)).auditMissingMovementEvent(any[AuthenticatedRequest[_]], eqTo(arrivalId))
        }
      }

      "must return Not Found when the arrival exists but does not share the channel" in {

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.getWithoutMessages(any(), eqTo(api))) thenReturn Future.successful(None)
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrivalId)(fakeRequest.withHeaders("channel" -> api.toString))

          status(result) mustBe NOT_FOUND
        }
      }

      "must return Ok when the arrival exists and shares the same channel" in {

        val arrival = arbitrary[ArrivalWithoutMessages].sample.value copy (eoriNumber = eoriNumber, channel = api)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.getWithoutMessages(any(), eqTo(api))) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrival.arrivalId)(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

          status(result) mustBe OK
        }
      }

      "must unlock the arrival and return Internal Server Error if the main action fails" in {

        val arrival = arbitrary[ArrivalWithoutMessages].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockArrivalMovementRepository    = mock[ArrivalMovementRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockArrivalMovementRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(Some(arrival))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.failingAction(arrival.arrivalId)(fakeRequest.withHeaders("channel" -> arrival.channel.toString))

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

      "must lock and unlock an arrival and return Forbidden" in {

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrivalId)(fakeRequest.withHeaders("channel" -> web.toString))

          status(result) mustBe FORBIDDEN
          verify(mockLockRepository, times(1)).lock(eqTo(arrivalId))
          verify(mockLockRepository, times(1)).unlock(eqTo(arrivalId))
        }
      }
    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockLockRepository = mock[LockRepository]

        when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

        val application = appBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrivalId)(fakeRequest)

          status(result) mustBe LOCKED
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

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector  = mock[AuthConnector]
        val mockLockRepository = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        val application = appBuilder
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrivalId)(fakeRequest.withHeaders(Headers.create()))

          status(result) mustEqual BAD_REQUEST
          contentAsString(result) mustEqual "Missing channel header or incorrect value specified in channel header"
        }
      }
    }

    "when channel header contains invalid value" - {

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

        val arrivalId = arbitrary[ArrivalId].sample.value

        val mockAuthConnector  = mock[AuthConnector]
        val mockLockRepository = mock[LockRepository]
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(true)

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = appBuilder
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(arrivalId)(fakeRequest.withHeaders("channel" -> "web2"))

          status(result) mustEqual BAD_REQUEST
          contentAsString(result) mustEqual "Missing channel header or incorrect value specified in channel header"
        }
      }
    }
  }
}
