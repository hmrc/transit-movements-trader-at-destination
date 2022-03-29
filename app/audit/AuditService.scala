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

import cats.data.Ior
import config.Constants

import javax.inject.Inject
import models._
import models.request.AuthenticatedRequest
import play.api.libs.json.Format.GenericFormat
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.MessageTranslation

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import utils.XMLTransformer.toJson

import scala.xml.NodeSeq

object AuditService {
  val maxRequestLength = 20000
}

class AuditService @Inject()(auditConnector: AuditConnector, messageTranslation: MessageTranslation)(implicit ec: ExecutionContext) {

  def auditEvent(auditType: String, enrolmentId: Ior[TURN, EORINumber], message: MovementMessage, channel: ChannelType)(implicit hc: HeaderCarrier): Unit = {
    val json    = messageTranslation.translate(toJson(message.message))
    val details = AuthenticatedAuditDetails(channel, customerId(enrolmentId), enrolmentType(enrolmentId), json)
    auditConnector.sendExplicitAudit(auditType, details)
  }

  def auditNCTSMessages(channel: ChannelType, customerId: String, messageResponse: MessageResponse, message: MovementMessage)(
    implicit hc: HeaderCarrier): Unit = {
    val json    = messageTranslation.translate(toJson(message.message))
    val details = UnauthenticatedAuditDetails(channel, customerId, json)
    auditConnector.sendExplicitAudit(messageResponse.auditType, details)
  }

  def auditNCTSRequestedMissingMovementEvent(arrivalId: ArrivalId, messageResponse: MessageResponse, message: MovementMessage)(
    implicit hc: HeaderCarrier): Unit = {
    val details = Json.obj(
      "arrivalId"           -> arrivalId,
      "messageResponseType" -> messageResponse.auditType,
      "message"             -> messageTranslation.translate(toJson(message.message))
    )
    auditConnector.sendExplicitAudit(AuditType.NCTSRequestedMissingMovement, details)
  }

  def auditCustomerRequestedMissingMovementEvent(request: AuthenticatedRequest[_], arrivalId: ArrivalId): Unit = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)
    val details =
      AuthenticatedAuditDetails(request.channel, customerId(request.enrolmentId), enrolmentType(request.enrolmentId), Json.obj("arrivalId" -> arrivalId))
    auditConnector.sendExplicitAudit(AuditType.CustomerRequestedMissingMovement, details)
  }

  def authAudit(auditType: String, details: AuthenticationDetails)(implicit hc: HeaderCarrier): Unit =
    auditConnector.sendExplicitAudit(auditType, details)

  def auditArrivalNotificationWithStatistics(auditType: String,
                                             enrolmentId: Ior[TURN, EORINumber],
                                             message: MovementMessage,
                                             channel: ChannelType,
                                             requestLength: Int,
                                             boxId: Option[BoxId])(implicit hc: HeaderCarrier): Unit = {

    lazy val statistics: JsObject = Json.obj(
      "authorisedLocationOfGoods" -> fieldValue(message.message, "ArrAutLocOfGooHEA65"),
      "totalNoOfContainers"       -> fieldOccurrenceCount(message.message, "CONNR3"),
      "requestLength"             -> requestLength
    )

    lazy val jsonMessage: JsObject =
      if (requestLength > AuditService.maxRequestLength) Json.obj("arrivalNotification" -> "Arrival notification too large to be included")
      else messageTranslation.translate(toJson(message.message))

    val details = ArrivalNotificationAuditDetails(channel, customerId(enrolmentId), enrolmentType(enrolmentId), jsonMessage, statistics, boxId)
    auditConnector.sendExplicitAudit(auditType, details)
  }

  private def fieldOccurrenceCount(message: NodeSeq, field: String): Int = (message \\ field).length

  private def fieldValue(message: NodeSeq, field: String): String =
    if (fieldOccurrenceCount(message, field) == 0) "NULL" else (message \\ field).text

  // Temporary until CTDA-1885 is merged

  def customerId(enrolmentId: Ior[TURN, EORINumber]): String =
    enrolmentId.fold(
      turn => turn.value,
      eoriNumber => eoriNumber.value,
      (_, eoriNumber) => eoriNumber.value
    )

  def enrolmentType(enrolmentId: Ior[TURN, EORINumber]): String =
    enrolmentId.fold(
      _ => Constants.LegacyEnrolmentKey,
      _ => Constants.NewEnrolmentKey,
      (_, _) => Constants.NewEnrolmentKey
    )
}
