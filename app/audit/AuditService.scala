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

import javax.inject.Inject
import models._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.JsonHelper

import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq
import AuditType._
import play.api.Logger
import play.api.libs.json.JsObject
import play.api.libs.json.Json

import scala.util.Failure
import scala.util.Success
import scala.util.Try

class AuditService @Inject()(auditConnector: AuditConnector)(implicit ec: ExecutionContext) {

  def auditEvent(auditType: String, xmlRequestBody: NodeSeq)(implicit hc: HeaderCarrier): Unit = {
    val eventualJson: Try[JsObject] = JsonHelper.convertXmlToJson(xmlRequestBody.toString())
    val convertedJson = eventualJson match {
      case Success(data) => data
      case Failure(error) =>
        Logger.error(s"Failed to convert xml to json with error: ${error.getMessage}")
        Json.obj()
    }

    val details = AuditDetails(convertedJson, xmlRequestBody.toString())
    auditConnector.sendExplicitAudit(auditType, Json.toJson(details))
  }

  def auditNCTSMessages(messageResponse: MessageResponse, xmlRequestBody: NodeSeq)(implicit hc: HeaderCarrier): Unit = {
    val auditType: String = messageResponse match {
      case GoodsReleasedResponse            => GoodsReleased
      case ArrivalRejectedResponse          => ArrivalNotificationRejected
      case UnloadingPermissionResponse      => UnloadingPermissionReceived
      case UnloadingRemarksRejectedResponse => UnloadingPermissionRejected
    }
    auditEvent(auditType, xmlRequestBody)
  }

}
