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

package controllers

import base.SpecBase
import connectors.MessageConnector
import generators.MessageGenerators
import models.request.ArrivalNotificationRequest
import models.request.InterchangeControlReference
import models.request.InternalReferenceId
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories._
import services._
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.Future

class ArrivalNotificationControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach with IntegrationPatience {

  private val mockMessageConnector: MessageConnector             = mock[MessageConnector]
  private val mockDatabaseService: DatabaseService               = mock[DatabaseService]
  private val mockSubmissionModelService: SubmissionModelService = mock[SubmissionModelService]
  private val mockXmlValidationService: XmlValidationService     = mock[XmlValidationService]

  private val application = {
    applicationBuilder
      .overrides(bind[MessageConnector].toInstance(mockMessageConnector))
      .overrides(bind[DatabaseService].toInstance(mockDatabaseService))
      .overrides(bind[SubmissionModelService].toInstance(mockSubmissionModelService))
      .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
      .build
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockMessageConnector)
    reset(mockDatabaseService)
    reset(mockSubmissionModelService)
    reset(mockXmlValidationService)
  }

  "post" - {

    "must return NO_CONTENT when successful" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>
          when(mockDatabaseService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right(XmlSuccessfullyValidated))

          when(mockDatabaseService.getInternalReferenceId)
            .thenReturn(Future.successful(Right(InternalReferenceId(0))))

          when(mockDatabaseService.saveArrivalNotification(any()))
            .thenReturn(Future.successful(Right(fakeWriteResult)))

          when(mockMessageConnector.post(any(), any(), any())(any(), any()))
            .thenReturn(Future.successful(HttpResponse(200)))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result = route(application, request).value

          status(result) mustEqual NO_CONTENT

      }
    }

    "must return INTERNAL_SERVER_ERROR when interchange control reference id cannot be generated" in {

      when(mockDatabaseService.getInterchangeControlReferenceId)
        .thenReturn(Future.successful(Left(FailedCreatingInterchangeControlReference)))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result: Future[Result] = route(application, request).value

      status(result) mustEqual INTERNAL_SERVER_ERROR
      contentAsJson(result) mustBe
        Json.obj("message" -> "failed to create InterchangeControlReference")
    }

    "must return BAD_REQUEST when conversion to request model fails" in {

      when(mockDatabaseService.getInterchangeControlReferenceId)
        .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

      when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
        .thenReturn(Left(FailedToConvertModel))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
      contentAsJson(result) mustBe
        Json.obj("message" -> "could not create request model")
    }

    "must return INTERNAL_SERVER_ERROR when internal reference id cannot be generated" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>
          when(mockDatabaseService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right(XmlSuccessfullyValidated))

          when(mockDatabaseService.getInternalReferenceId)
            .thenReturn(Future.successful(Left(FailedCreatingNextInternalReferenceId)))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result: Future[Result] = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          contentAsJson(result) mustBe
            Json.obj("message" -> "failed to create InternalReferenceId")

      }
    }

    "must return INTERNAL_SERVER_ERROR when saving to database returns FailedSavingArrivalNotification" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>
          when(mockDatabaseService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right(XmlSuccessfullyValidated))

          when(mockDatabaseService.getInternalReferenceId)
            .thenReturn(Future.successful(Right(InternalReferenceId(0))))

          when(mockDatabaseService.saveArrivalNotification(any()))
            .thenReturn(Future.successful(Left(FailedSavingArrivalNotification)))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result: Future[Result] = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          contentAsJson(result) mustBe
            Json.obj("message" -> "failed to save an Arrival Notification to Database")

      }
    }

    "must return INTERNAL_SERVER_ERROR when saving to database fails" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>
          when(mockDatabaseService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right(XmlSuccessfullyValidated))

          when(mockDatabaseService.getInternalReferenceId)
            .thenReturn(Future.successful(Right(InternalReferenceId(0))))

          when(mockDatabaseService.saveArrivalNotification(any()))
            .thenReturn(Future.failed(new BadRequestException("")))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result: Future[Result] = route(application, request).value

          whenReady(result) {
            result =>
              result.header.status mustBe INTERNAL_SERVER_ERROR
          }
      }
    }

    "must return BAD_GATEWAY when post fails" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>
          when(mockDatabaseService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right(XmlSuccessfullyValidated))

          when(mockDatabaseService.getInternalReferenceId)
            .thenReturn(Future.successful(Right(InternalReferenceId(0))))

          when(mockDatabaseService.saveArrivalNotification(any()))
            .thenReturn(Future.successful(Right(fakeWriteResult)))

          when(mockMessageConnector.post(any(), any(), any())(any(), any()))
            .thenReturn(Future.failed(new BadRequestException("")))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result: Future[Result] = route(application, request).value

          status(result) mustEqual BAD_GATEWAY
          contentAsJson(result) mustBe
            Json.obj("message" -> "failed submission to EIS")
      }
    }

    "must return BAD_REQUEST when xml validation fails" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>
          when(mockDatabaseService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertToSubmissionModel(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Left(FailedToValidateXml("missing element")))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          contentAsJson(result) mustBe
            Json.obj("message" -> "Xml validation failed for the following reason: missing element")
      }
    }

    "must return BAD_REQUEST when given invalid json" in {

      val invalidJson = Json.parse("""{
          |"invalid" : "json"
          |}
        """.stripMargin)

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(invalidJson))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }
  }

}
