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

package audit

import base.SpecBase
import cats.data.Ior
import models.ChannelType.api
import models._
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.Helpers.running
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import config.Constants
import org.scalacheck.Gen
import models.request.AuthenticatedRequest
import org.scalacheck.Arbitrary.arbitrary
import play.api.test.FakeRequest
import utils.Format
import utils.MessageTranslation
import utils.XMLTransformer.toJson

class AuditServiceSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuditConnector)
  }

  "AuditService" - {
    "must audit notification message event" in {
      val requestEori  = Ior.right(EORINumber("eori"))
      val requestXml   = <xml>test</xml>
      val auditDetails = AuthenticatedAuditDetails(ChannelType.api, requestEori, Json.obj("xml" -> "test"))

      val movementMessage =
        MovementMessageWithStatus(MessageId(1), LocalDateTime.now, MessageType.ArrivalNotification, requestXml, MessageStatus.SubmissionSucceeded, 1)

      val auditType = "Some AuditEvent"

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()
      running(application) {
        val auditService = application.injector.instanceOf[AuditService]
        auditService.auditEvent(auditType, requestEori, movementMessage, api)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType), eqTo(auditDetails))(any(), any(), any())
      }
    }

    "must audit NCTS message GoodsReleasedResponse event" in {
      val requestEori  = "eori"
      val requestXml   = <xml>test</xml>
      val auditDetails = UnauthenticatedAuditDetails(ChannelType.api, requestEori, Json.obj("xml" -> "test"))

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.GoodsReleased, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(channel = api, customerId = requestEori, GoodsReleasedResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.GoodsReleased), eqTo(auditDetails))(any(), any(), any())
      }
    }

    "must audit NCTS message ArrivalRejectedResponse event" in {
      val requestEori  = "eori"
      val requestXml   = <xml>test</xml>
      val auditDetails = UnauthenticatedAuditDetails(ChannelType.api, requestEori, Json.obj("xml" -> "test"))

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.ArrivalRejection, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(channel = api, customerId = requestEori, ArrivalRejectedResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.ArrivalNotificationRejected), eqTo(auditDetails))(any(), any(), any())
      }
    }

    "must audit NCTS message UnloadingPermissionResponse event" in {
      val requestEori  = "eori"
      val requestXml   = <xml>test</xml>
      val auditDetails = UnauthenticatedAuditDetails(ChannelType.api, requestEori, Json.obj("xml" -> "test"))

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.UnloadingPermission, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(api, requestEori, UnloadingPermissionResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.UnloadingPermissionReceived), eqTo(auditDetails))(any(), any(), any())
      }

    }

    "must audit NCTS message UnloadingRemarksRejectedResponse event" in {
      val requestEori  = "eori"
      val requestXml   = <xml>test</xml>
      val auditDetails = UnauthenticatedAuditDetails(ChannelType.api, requestEori, Json.obj("xml" -> "test"))

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.UnloadingRemarksRejection, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(channel = api, customerId = requestEori, UnloadingRemarksRejectedResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.UnloadingPermissionRejected), eqTo(auditDetails))(any(), any(), any())
      }

    }

    "must audit auth events" in {

      forAll(Gen.oneOf(ChannelType.values), Gen.oneOf(Seq(Constants.LegacyEnrolmentIdKey, Constants.NewEnrolmentIdKey))) {
        (channel, enrolmentType) =>
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()

          running(application) {
            val auditService = application.injector.instanceOf[AuditService]
            val details      = AuthenticationDetails(channel, enrolmentType)
            auditService.authAudit(AuditType.SuccessfulAuthTracking, details)

            verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.SuccessfulAuthTracking), eqTo(details))(any(), any(), any())
            reset(mockAuditConnector)
          }

      }
    }

    "must audit customer missing movement events" in {
      forAll(Gen.oneOf(ChannelType.values)) {
        (channel) =>
          val request = new AuthenticatedRequest[Any](FakeRequest(), channel, Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()

          running(application) {
            val auditService    = application.injector.instanceOf[AuditService]
            val arrivalId       = ArrivalId(1234)
            val expectedDetails = AuthenticatedAuditDetails(request.channel, request.enrolmentId, Json.obj("arrivalId" -> arrivalId))
            auditService.auditCustomerRequestedMissingMovementEvent(request, arrivalId)

            verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.CustomerRequestedMissingMovement), eqTo(expectedDetails))(any(), any(), any())
            reset(mockAuditConnector)
          }

      }
    }

    "must audit NCTS missing movement events" in {
      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()
      val messageResponse = UnloadingPermissionResponse
      val requestXml      = <xml>test</xml>
      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.UnloadingPermission, requestXml, 1)
      val arrivalId       = ArrivalId(0)
      val expectedDetails = Json.obj(
        "arrivalId"           -> arrivalId,
        "messageResponseType" -> messageResponse.auditType,
        "message"             -> toJson(movementMessage.message)
      )

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]
        auditService.auditNCTSRequestedMissingMovementEvent(arrivalId, messageResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTSRequestedMissingMovement), eqTo(expectedDetails))(any(), any())
        reset(mockAuditConnector)
      }

    }

    "must adjust declaration audit according to request size" - {

      val auditType = "Arrival audit"
      val xml       = <CC007A>test</CC007A>
      val movementMessage =
        MovementMessageWithStatus(MessageId(1), LocalDateTime.now, MessageType.ArrivalNotification, xml, MessageStatus.SubmissionSucceeded, 1)

      val statistics = (contentLength: Int) =>
        Json.obj(
          "Declaration request content-length" -> contentLength
      )

      "when the request is smaller than max size allowed the xml and statistics should be included in the audit detail" in {

        val contentLength = 1000
        val request       = new AuthenticatedRequest[Any](FakeRequest(), api, Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))

        val application = baseApplicationBuilder
          .configure("message-translation-file" -> "MessageTranslation.json")
          .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
          .build()

        running(application) {
          val auditService       = application.injector.instanceOf[AuditService]
          val messageTranslation = application.injector.instanceOf[MessageTranslation]

          val jsonMessage = messageTranslation.translate(toJson(movementMessage.message))

          val expectedDetails = AuthenticatedAuditDetails(request.channel, request.enrolmentId, jsonMessage ++ statistics(contentLength))

          auditService.auditArrivalWithStatistics(contentLength, auditType, request.enrolmentId, movementMessage, request.channel)

          verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType.toString), eqTo(expectedDetails))(any(), any(), any())
          reset(mockAuditConnector)
        }
      }

      "when the request is larger than max size allowed the audit detail should include statistics and replace translated xml with message" in {

        val contentLength = 60000
        val request       = new AuthenticatedRequest[Any](FakeRequest(), api, Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))

        val json = Json.obj(
          "Declaration Data" -> "The declaration data was too large to be included",
        )

        val application = baseApplicationBuilder
          .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
          .build()
        running(application) {

          val expectedDetails = AuthenticatedAuditDetails(request.channel, request.enrolmentId, json ++ statistics(contentLength))

          val auditService = application.injector.instanceOf[AuditService]
          auditService.auditArrivalWithStatistics(contentLength, auditType, request.enrolmentId, movementMessage, request.channel)

          verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType.toString), eqTo(expectedDetails))(any(), any(), any())
          reset(mockAuditConnector)
        }
      }
    }
  }
}
