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
import generators.MessageGenerators
import models.messages.request.{FailedToConvert, FailedToCreateXml, FailedToValidateXml}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.SubmissionService
import uk.gov.hmrc.http.{BadGatewayException, GatewayTimeoutException, HttpResponse, UnauthorizedException}

import scala.concurrent.Future


class ArrivalNotificationControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach {

  private val mockSubmissionService: SubmissionService = mock[SubmissionService]
  private val application = {
    applicationBuilder
      .overrides(bind[SubmissionService].toInstance(mockSubmissionService)).build()
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockSubmissionService)
  }

  "post" - {

    "must return BAD_GATEWAY when the EIS service is down" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Right(Future.failed(new BadGatewayException(""))))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      whenReady(result.failed) {
        _ mustBe an[BadGatewayException]
      }
    }

    "must return UNAUTHORIZED when user is unauthenticated" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Right(Future.failed(new UnauthorizedException(""))))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      whenReady(result.failed) {
        _ mustBe an[UnauthorizedException]
      }
    }

    "must return GATEWAY_TIMEOUT" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Right(Future.failed(new GatewayTimeoutException(""))))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      whenReady(result.failed) {
        _ mustBe an[GatewayTimeoutException]
      }
    }

    "must return OK when passed valid NormalNotification" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Right(Future.successful(HttpResponse(OK))))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual OK
    }

    "must return a BadRequest when conversion to xml has failed" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Left(FailedToCreateXml))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }

    "must return a BadRequest when conversion to request model has failed" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Left(FailedToConvert))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }

    "must return a BadRequest when xml validation has failed" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Left(FailedToValidateXml))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(normalNotification))

      val result = route(application, request).value

      status(result) mustEqual BAD_REQUEST
    }
  }

}
