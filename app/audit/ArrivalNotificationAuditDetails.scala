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

import models.BoxId
import models.ChannelType
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import utils.MessageTranslation
import utils.XMLTransformer.toJson

import scala.xml.NodeSeq

case class ArrivalNotificationAuditDetails(channel: ChannelType,
                                           customerId: String,
                                           enrolmentType: String,
                                           message: NodeSeq,
                                           requestLength: Int,
                                           boxId: Option[BoxId],
                                           messageTranslation: MessageTranslation) {

  def fieldOccurrenceCount(field: String): Int = (message \\ field).length
  def fieldValue(field: String): String        = if (fieldOccurrenceCount(field) == 0) "NULL" else (message \\ field).text

  lazy val statistics: JsObject = Json.obj(
    "authorisedLocationOfGoods" -> fieldValue("ArrAutLocOfGooHEA65"),
    "totalNoOfContainers"       -> fieldOccurrenceCount("CONNR3"),
    "requestLength"             -> requestLength
  )

  lazy val declaration: JsObject =
    if (requestLength > ArrivalNotificationAuditDetails.maxRequestLength) Json.obj("arrivalNotification" -> "Arrival notification too large to be included")
    else messageTranslation.translate(toJson(message))

}

object ArrivalNotificationAuditDetails {

  val maxRequestLength = 20000

  implicit val writes: OWrites[ArrivalNotificationAuditDetails] = (details: ArrivalNotificationAuditDetails) => {
    Json.obj(
      "channel"       -> details.channel,
      "customerId"    -> details.customerId,
      "enrolmentType" -> details.enrolmentType,
      "message"       -> details.declaration,
      "statistics"    -> details.statistics
    ) ++ details.boxId.fold(JsObject.empty)(id => Json.obj("boxId" -> id))
  }
}
