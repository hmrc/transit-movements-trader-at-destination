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

import play.api.libs.json.JsObject
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import utils.JsonHelper

import scala.xml.NodeSeq

object ArrivalNotificationSubmissionEvent {

  def apply(requestBody: NodeSeq)(implicit hc: HeaderCarrier): ExtendedDataEvent =
    ExtendedDataEvent(
      auditSource = "",
      auditType = "ArrivalNotificationSubmission",
      tags = hc.toAuditTags("ArrivalNotificationSubmission", "N/A"),
      detail = JsonHelper.convertXmlToJson(requestBody.toString()).as[JsObject]
        ++ Json.toJson(hc.toAuditDetails()).as[JsObject]
    )
}
