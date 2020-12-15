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

import audit.AuditType._
import javax.inject.Inject
import models._
import play.api.libs.json.Format.GenericFormat
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.JsonHelper

import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq

class AuditService @Inject()(auditConnector: AuditConnector, jsonHelper: JsonHelper)(implicit ec: ExecutionContext) {

  def auditEvent(auditType: String, xmlRequestBody: NodeSeq, channel: ChannelType)(implicit hc: HeaderCarrier): Unit = {
    val json: JsObject = jsonHelper.convertXmlToJson(xmlRequestBody.toString())

    val details = AuditDetails(channel, json)
    auditConnector.sendExplicitAudit(auditType, Json.toJson(details))
  }

  def auditNCTSMessages(arrival: Arrival, messageResponse: MessageResponse, xmlRequestBody: NodeSeq)(implicit hc: HeaderCarrier): Unit = {
    val auditType: String = messageResponse match {
      case GoodsReleasedResponse            => GoodsReleased
      case ArrivalRejectedResponse          => ArrivalNotificationRejected
      case UnloadingPermissionResponse      => UnloadingPermissionReceived
      case UnloadingRemarksRejectedResponse => UnloadingPermissionRejected
    }
    auditEvent(auditType, xmlRequestBody, arrival.channel)
  }

}
