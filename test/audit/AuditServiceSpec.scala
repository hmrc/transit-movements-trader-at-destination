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

package audit

import base.SpecBase
import models.ArrivalRejectedResponse
import models.GoodsReleasedResponse
import models.UnloadingPermissionResponse
import models.UnloadingRemarksRejectedResponse
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.JsObject
import play.api.test.Helpers.running
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

class AuditServiceSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuditConnector)
  }

  "AuditService" - {
    "must audit notification messages event" in {

      val requestXml = <xml>test</xml>

      val auditType = "Some AuditEvent"

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()
      running(application) {
        val auditService = application.injector.instanceOf[AuditService]
        auditService.auditEvent(auditType, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType), any[JsObject]())(any(), any())
      }
    }

    "must audit NCTS message GoodsReleasedResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(GoodsReleasedResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.GoodsReleased.toString), any[JsObject]())(any(), any())
      }
    }

    "must audit NCTS message ArrivalRejectedResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(ArrivalRejectedResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.ArrivalNotificationRejected.toString), any[JsObject]())(any(), any())
      }
    }

    "must audit NCTS message UnloadingPermissionResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(UnloadingPermissionResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.UnloadingPermissionReceived.toString), any[JsObject]())(any(), any())
      }

    }

    "must audit NCTS message UnloadingRemarksRejectedResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(UnloadingRemarksRejectedResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.UnloadingPermissionRejected.toString), any[JsObject]())(any(), any())
      }

    }
  }

}
