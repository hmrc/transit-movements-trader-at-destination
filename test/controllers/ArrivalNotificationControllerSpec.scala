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

package controllers

import base.SpecBase
import connectors.MessageConnector
import generators.MessageGenerators
import models.messages.request.{ArrivalNotificationRequest, FailedToConvert, FailedToCreateXml, FailedToValidateXml, InterchangeControlReference}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.mvc.Results.{NoContent, InternalServerError}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories._
import services.{FailedCreatingInterchangeControlReference, InterchangeControlReferenceService, SubmissionModelService, XmlBuilderService, XmlSubmissionService, XmlValidationService}
import uk.gov.hmrc.http.{BadRequestException, HttpResponse}

import scala.concurrent.Future
import scala.xml.Node


class ArrivalNotificationControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach {

  private val mockSubmissionService: XmlSubmissionService = mock[XmlSubmissionService]
  // BELOW IS UNUSED
  //  private val mockSequentialInterchangeControlReferenceIdRepository: SequentialInterchangeControlReferenceIdRepository = mock[SequentialInterchangeControlReferenceIdRepository]
  private val mockArrivalNotificationRepository: ArrivalNotificationRepository = mock[ArrivalNotificationRepository]
  private val mockMessageConnector: MessageConnector = mock[MessageConnector]
  private val mockInterchangeControlReferenceService: InterchangeControlReferenceService = mock[InterchangeControlReferenceService]
  private val mockSubmissionModelService: SubmissionModelService = mock[SubmissionModelService]
  private val mockXmlBuilderService: XmlBuilderService = mock[XmlBuilderService]
  private val mockXmlValidationService: XmlValidationService = mock[XmlValidationService]


  private val application = {
    applicationBuilder
      .overrides(bind[XmlSubmissionService].toInstance(mockSubmissionService))
      //      .overrides(bind[SequentialInterchangeControlReferenceIdRepository].toInstance(mockSequentialInterchangeControlReferenceIdRepository))
      .overrides(bind[ArrivalNotificationRepository].toInstance(mockArrivalNotificationRepository))
      .overrides(bind[MessageConnector].toInstance(mockMessageConnector))
      .overrides(bind[InterchangeControlReferenceService].toInstance(mockInterchangeControlReferenceService))
      .overrides(bind[SubmissionModelService].toInstance(mockSubmissionModelService))
      .overrides(bind[XmlBuilderService].toInstance(mockXmlBuilderService))
      .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService)
      ).build
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockSubmissionService)
    //    reset(mockSequentialInterchangeControlReferenceIdRepository)
    reset(mockArrivalNotificationRepository)
    reset(mockMessageConnector)
    reset(mockInterchangeControlReferenceService)
    reset(mockSubmissionModelService)
    reset(mockXmlBuilderService)
    reset(mockXmlValidationService)

  }

  private val testNode: Node = <element1>test</element1>

  "post" - {

    "must return NO_CONTENT when passed valid NormalNotification" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>

          when(mockInterchangeControlReferenceService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertFromArrivalNotification(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlBuilderService.buildXml(any())(any()))
            .thenReturn(Right(testNode))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right((): Unit))

          when(mockArrivalNotificationRepository.persistToMongo(any()))
              .thenReturn(Future.successful(fakeWriteResult))

          when(mockMessageConnector.post(any(), any(), any())(any(), any()))
            .thenReturn(Future.successful(NoContent))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result = route(application, request).value

          status(result) mustEqual NO_CONTENT
      }
    }

    "must return a future failed when interchange control reference id cannot be generated" in {

      when(mockInterchangeControlReferenceService.getInterchangeControlReferenceId)
        .thenReturn(Future.successful(Left(FailedCreatingInterchangeControlReference)))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result: Future[Result] = route(application, request).value

      status(result) mustEqual INTERNAL_SERVER_ERROR
    }

    "must return a future failed when persist to mongo fails" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>

          when(mockInterchangeControlReferenceService.getInterchangeControlReferenceId)
            .thenReturn(Future.successful(Right(InterchangeControlReference("20190101", 1))))

          when(mockSubmissionModelService.convertFromArrivalNotification(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlBuilderService.buildXml(any())(any()))
            .thenReturn(Right(testNode))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Left(FailedToValidateXml))

          when(mockArrivalNotificationRepository.persistToMongo(any()))
            .thenReturn(Future.failed(new BadRequestException("")))

          val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
            .withJsonBody(Json.toJson(normalNotification))

          val result: Future[Result] = route(application, request).value

          whenReady(result) {
            result =>
              result.header.status mustBe 500
          }
      }
    }
    "must return a future failed when POST fails" in  {

      //      when(mockSequentialInterchangeControlReferenceIdRepository.nextInterchangeControlReferenceId())
      //        .thenReturn(Future.successful(InterchangeControlReference("20190101", 1)))

      when(mockSubmissionService.buildAndValidateXml(any(), any())(any(), any()))
        .thenReturn(Right(testNode))

      //      when(mockArrivalNotificationRepository.persistToMongo(any()))
      //        .thenReturn(Future.failed(new BadRequestException("")))

      when(mockMessageConnector.post(any(), any(), any())(any(), any()))
        .thenReturn(Future.failed(new BadRequestException("")))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result: Future[Result] = route(application, request).value

      whenReady(result.failed) {
        _ mustBe an[BadRequestException]

      }
    }

    "must return a BadRequest when conversion to xml has failed" in {

      //      when(mockSequentialInterchangeControlReferenceIdRepository.nextInterchangeControlReferenceId())
      //        .thenReturn(Future.successful(InterchangeControlReference("20190101", 1)))

      when(mockSubmissionService.buildAndValidateXml(any(), any())(any(), any()))
        .thenReturn(Left(FailedToCreateXml))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }

    "must return a BadRequest when conversion to request model has failed" in {

      //      when(mockSequentialInterchangeControlReferenceIdRepository.nextInterchangeControlReferenceId())
      //        .thenReturn(Future.successful(InterchangeControlReference("20190101", 1)))

      when(mockSubmissionService.buildAndValidateXml(any(), any())(any(), any()))
        .thenReturn(Left(FailedToConvert))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }

    "must return a BadRequest when xml validation has failed" ignore {

      //      when(mockSequentialInterchangeControlReferenceIdRepository.nextInterchangeControlReferenceId())
      //        .thenReturn(Future.successful(InterchangeControlReference("20190101", 1)))

      when(mockSubmissionService.buildAndValidateXml(any(), any())(any(), any()))
        .thenReturn(Left(FailedToValidateXml))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }
  }

}
