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

import java.time.{LocalDate, LocalDateTime}

import generators.MessageGenerators
import models.TraderWithEori
import models.messages.request.FailedToValidateXml
import models.messages.{ArrivalNotification, NormalNotification}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterEach, FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.SubmissionService
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import org.mockito.Matchers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class ArrivalNotificationControllerSpec extends
  FreeSpec with
  MustMatchers with
  ScalaCheckPropertyChecks with
  MessageGenerators with
  GuiceOneAppPerSuite with
  OptionValues with
  MockitoSugar with
  BeforeAndAfterEach with ScalaFutures {

  /**
    * SHOULD
    * Return 200 on successful conversion to ArrivalNotification
    * Return 400 when ArrivalNotification could not be built
    * Return 401 when user isn't authenticated
    * Return 502 if EIS is down
    * Return 504 (check this doesn't happen automatically
    */

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mockSubmissionService: SubmissionService = mock[SubmissionService]

  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .overrides(
      bind[SubmissionService].toInstance(mockSubmissionService)
    ).build()

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockSubmissionService)
  }

  val stuff = NormalNotification("test", "test", LocalDate.now(), None, TraderWithEori("woa", None, None, None, None, None), "sadsf", Seq.empty)

  "post" - {
    
    "must return BAD_GATEWAY when the EIS service is down" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Right(Future.failed(new Exception("woa"))))


      intercept[Exception] {

        val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
          .withJsonBody(Json.toJson(stuff))

        route(app, request).value
      }
    }

    "must return OK when passed valid NormalNotification" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Right(Future.successful(HttpResponse(OK))))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(stuff))

      val result = route(app, request).value

      status(result) mustEqual OK
    }

    "summit" in {

      when(mockSubmissionService.submit(any())(any(), any()))
        .thenReturn(Left(FailedToValidateXml))

      val request = FakeRequest(POST, routes.ArrivalNotificationController.post().url)
        .withJsonBody(Json.toJson(stuff))

      val result = route(app, request).value

      status(result) mustEqual 400
    }
  }

}
