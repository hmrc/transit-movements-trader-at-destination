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

package services

import base.SpecBase
import config.Constants
import connectors.PushPullNotificationConnector
import generators.ModelGenerators
import models.MessageType.GoodsReleased
import models._
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers._
import org.mockito.BDDMockito._
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.verifyNoInteractions
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.Headers
import play.api.test.Helpers._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.UpstreamErrorResponse
import utils.Format

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class PushPullNotificationServiceSpec extends SpecBase with BeforeAndAfterEach with ScalaCheckPropertyChecks with ModelGenerators {

  val mockConnector = mock[PushPullNotificationConnector]
  val service       = new PushPullNotificationService(mockConnector)

  override protected def beforeEach(): Unit = reset(mockConnector)

  "PushPullNotificationService" - {

    val testBoxId    = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
    val testClientId = "X5ZasuQLH0xqKooV_IEw6yjQNfEa"
    val testBox      = Box(BoxId(testBoxId), Constants.BoxName)

    "getBox" - {

      "return a Some(box) when connector call returns 200" in {
        val mockedGetBox     = mockConnector.getBox(anyString())(any[ExecutionContext], any[HeaderCarrier])
        val successfulResult = Future.successful(Right(testBox))

        given(mockedGetBox).willReturn(successfulResult)

        Await.result(service.getBox(testClientId), 30.seconds).mustEqual(Some(testBox))
      }

      "return None when any 4xx or 5xx Http status returned" in {
        val errorGenerator: Gen[Int] = Gen.oneOf(
          Seq(
            INTERNAL_SERVER_ERROR,
            BAD_REQUEST,
            FORBIDDEN,
            GATEWAY_TIMEOUT,
            NOT_FOUND,
            NOT_IMPLEMENTED,
            SERVICE_UNAVAILABLE,
            UNAUTHORIZED
          )
        )

        forAll(errorGenerator) {
          code =>
            val mockedGetBox = mockConnector.getBox(anyString())(any[ExecutionContext], any[HeaderCarrier])
            val testMessage  = "this is a test message"
            val failedResult = Future.successful(Left(UpstreamErrorResponse(testMessage, code)))

            given(mockedGetBox).willReturn(failedResult)

            Await.result(service.getBox(testClientId), 30.seconds).mustEqual(None)
        }
      }
    }

    "sendPushNotification" - {

      val smallContentLengthHeader = (CONTENT_LENGTH, "1000") // arbitrary, but will trigger attaching a body to the message.
      val largeContentLengthHeader = (CONTENT_LENGTH, "850000") // arbitrary, but is too large to trigger attaching a body to the message.
      val arrival                  = arbitrary[ArrivalWithoutMessages].sample.value.copy(notificationBox = Some(testBox))

      "should return a unit value when connector call succeeds" in {

        val dateOfPrep: LocalDate = LocalDate.now()
        val timeOfPrep: LocalTime = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val successfulResult: Future[Either[UpstreamErrorResponse, Unit]] = Future.successful(Right(()))

        when(
          mockConnector.postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
        ).thenReturn(successfulResult)

        val movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          LocalDateTime.now(),
          GoodsReleased,
          requestXmlBody,
          0
        )

        val inboundMessageRequest = InboundMessageRequest(arrival, GoodsReleasedResponse, movementMessage)

        Await.result(service.sendPushNotification(inboundMessageRequest, Headers(("key", "value"), smallContentLengthHeader)), 30.seconds).mustEqual(())

        verify(mockConnector).postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext],
                                                                                                                       any[HeaderCarrier])
      }

      "should return a unit value when connector call fails with a 413, then succeeds after stripping the message, and calls PPNS twice" in {
        val dateOfPrep: LocalDate = LocalDate.now()
        val timeOfPrep: LocalTime = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val successfulResult: Future[Either[UpstreamErrorResponse, Unit]] = Future.successful(Right(()))
        val failResult: Future[Either[UpstreamErrorResponse, Unit]] =
          Future.successful(Left(UpstreamErrorResponse("Request Entity Too Large", REQUEST_ENTITY_TOO_LARGE)))

        when(
          mockConnector.postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
        ).thenAnswer {
          invocation =>
            if (invocation.getArgument[ArrivalMessageNotification](1).messageBody.isDefined) failResult
            else successfulResult
        }

        val movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          LocalDateTime.now(),
          GoodsReleased,
          requestXmlBody,
          0
        )

        val inboundMessageRequest = InboundMessageRequest(arrival, GoodsReleasedResponse, movementMessage)

        Await.result(service.sendPushNotification(inboundMessageRequest, Headers(("key", "value"), smallContentLengthHeader)), 30.seconds).mustEqual(())

        verify(mockConnector, times(2))
          .postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
      }

      "should return a unit value when connector call fails with a 500, and only call PPNS once" in {
        val dateOfPrep: LocalDate = LocalDate.now()
        val timeOfPrep: LocalTime = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val failResult: Future[Either[UpstreamErrorResponse, Unit]] =
          Future.successful(Left(UpstreamErrorResponse("Internal Server Error", INTERNAL_SERVER_ERROR)))

        when(
          mockConnector.postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
        ).thenReturn(failResult)

        val movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          LocalDateTime.now(),
          GoodsReleased,
          requestXmlBody,
          0
        )

        val inboundMessageRequest = InboundMessageRequest(arrival, GoodsReleasedResponse, movementMessage)

        Await.result(service.sendPushNotification(inboundMessageRequest, Headers(("key", "value"), smallContentLengthHeader)), 30.seconds).mustEqual(())

        verify(mockConnector).postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext],
                                                                                                                       any[HeaderCarrier])
        verify(mockConnector)
          .postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
      }

      "should return a unit value when connector call fails with a 413 and has no message to begin with, and only calls PPNS once" in {
        val dateOfPrep: LocalDate = LocalDate.now()
        val timeOfPrep: LocalTime = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val failResult: Future[Either[UpstreamErrorResponse, Unit]] =
          Future.successful(Left(UpstreamErrorResponse("Request Entity Too Large", REQUEST_ENTITY_TOO_LARGE)))

        when(
          mockConnector.postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
        ).thenReturn(failResult)

        val movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          LocalDateTime.now(),
          GoodsReleased,
          requestXmlBody,
          0
        )

        val inboundMessageRequest = InboundMessageRequest(arrival, GoodsReleasedResponse, movementMessage)

        Await.result(service.sendPushNotification(inboundMessageRequest, Headers(("key", "value"), largeContentLengthHeader)), 30.seconds).mustEqual(())
        verify(mockConnector)
          .postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
      }

      "should not post if no box exists" in {

        val arrivalWithoutBox = arrival.copy(notificationBox = None)

        val requestXmlBody = <CC025A></CC025A>

        val successfulResult: Future[Either[UpstreamErrorResponse, Unit]] = Future.successful(Right(()))

        when(
          mockConnector.postNotification(BoxId(ArgumentMatchers.eq(testBoxId)), any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])
        ).thenReturn(successfulResult)

        val movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          LocalDateTime.now(),
          GoodsReleased,
          requestXmlBody,
          0
        )

        val inboundMessageRequest = InboundMessageRequest(arrivalWithoutBox, GoodsReleasedResponse, movementMessage)

        Await.result(service.sendPushNotification(inboundMessageRequest, Headers(("key", "value"), smallContentLengthHeader)), 30.seconds).mustEqual(())

        verifyNoInteractions(mockConnector)
      }

      "should return a unit value when connector call fails" in {

        val dateOfPrep: LocalDate = LocalDate.now()
        val timeOfPrep: LocalTime = LocalTime.of(1, 1)

        val requestXmlBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val boxIdMatcher = refEq(testBoxId).asInstanceOf[BoxId]

        val mockedPostNotification = mockConnector.postNotification(boxIdMatcher, any[ArrivalMessageNotification])(any[ExecutionContext], any[HeaderCarrier])

        given(mockedPostNotification).willReturn(Future.failed(new RuntimeException))

        val movementMessage = MovementMessageWithoutStatus(
          MessageId(0),
          LocalDateTime.now(),
          GoodsReleased,
          requestXmlBody,
          0
        )

        val inboundMessageRequest = InboundMessageRequest(arrival, GoodsReleasedResponse, movementMessage)

        Await.result(service.sendPushNotification(inboundMessageRequest, Headers(("key", "value"), smallContentLengthHeader)), 30.seconds).mustEqual(())
      }
    }
  }
}
