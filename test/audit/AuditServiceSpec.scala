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

package audit

import java.time.LocalDateTime

import base.SpecBase
import models._
import models.ChannelType.api
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

class AuditServiceSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuditConnector)
  }

  "AuditService" - {
    "must audit notification message event" in {
      val requestEori        = "eori"
      val requestXml         = <xml>test</xml>
      val requestedXmlToJson = Json.parse("{\"channel\":\"api\",\"customerId\":\"eori\",\"xml\":\"test\"}")

      val movementMessage =
        MovementMessageWithStatus(MessageId(1), LocalDateTime.now, MessageType.ArrivalNotification, requestXml, MessageStatus.SubmissionSucceeded, 1)

      val auditType = "Some AuditEvent"

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()
      running(application) {
        val auditService = application.injector.instanceOf[AuditService]
        auditService.auditEvent(auditType, requestEori, movementMessage, api)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType), eqTo(requestedXmlToJson))(any(), any(), any())
      }
    }

    "must audit NCTS message GoodsReleasedResponse event" in {
      val requestEori        = "eori"
      val requestXml         = <xml>test</xml>
      val requestedXmlToJson = Json.parse("{\"channel\":\"api\",\"customerId\":\"eori\",\"xml\":\"test\"}")

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.GoodsReleased, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(channel = api, customerId = requestEori, GoodsReleasedResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.GoodsReleased), eqTo(requestedXmlToJson))(any(), any(), any())
      }
    }

    "must audit NCTS message ArrivalRejectedResponse event" in {
      val requestEori        = "eori"
      val requestXml         = <xml>test</xml>
      val requestedXmlToJson = Json.parse("{\"channel\":\"api\",\"customerId\":\"eori\",\"xml\":\"test\"}")

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.ArrivalRejection, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(channel = api, customerId = requestEori, ArrivalRejectedResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.ArrivalNotificationRejected), eqTo(requestedXmlToJson))(any(), any(), any())
      }
    }

    "must audit NCTS message UnloadingPermissionResponse event" in {
      val requestEori        = "eori"
      val requestXml         = <xml>test</xml>
      val requestedXmlToJson = Json.parse("{\"channel\":\"api\",\"customerId\":\"eori\",\"xml\":\"test\"}")

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.UnloadingPermission, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(api, requestEori, UnloadingPermissionResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.UnloadingPermissionReceived), eqTo(requestedXmlToJson))(any(), any(), any())
      }

    }

    "must audit NCTS message UnloadingRemarksRejectedResponse event" in {
      val requestEori        = "eori"
      val requestXml         = <xml>test</xml>
      val requestedXmlToJson = Json.parse("{\"channel\":\"api\",\"customerId\":\"eori\",\"xml\":\"test\"}")

      val movementMessage = MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, MessageType.UnloadingRemarksRejection, requestXml, 1)

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(channel = api, customerId = requestEori, UnloadingRemarksRejectedResponse, movementMessage)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.UnloadingPermissionRejected), eqTo(requestedXmlToJson))(any(), any(), any())
      }

    }
  }

}
