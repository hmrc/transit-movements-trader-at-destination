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
import javax.inject.Inject
import models._
import models.request.AuthenticatedRequest
import play.api.libs.json.Format.GenericFormat
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.MessageTranslation

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.play.http.HeaderCarrierConverter

class AuditService @Inject()(auditConnector: AuditConnector, messageTranslation: MessageTranslation)(implicit ec: ExecutionContext) {

  def auditEvent(auditType: String, enrolmentId: Ior[TURN, EORINumber], message: MovementMessage, channel: ChannelType)(implicit hc: HeaderCarrier): Unit = {
    val json    = messageTranslation.translate(message.messageJson)
    val details = AuthenticatedAuditDetails(channel, enrolmentId, json)
    auditConnector.sendExplicitAudit(auditType, details)
  }

  def auditNCTSMessages(channel: ChannelType, customerId: String, messageResponse: MessageResponse, message: MovementMessage)(
    implicit hc: HeaderCarrier): Unit = {
    val json    = messageTranslation.translate(message.messageJson)
    val details = UnauthenticatedAuditDetails(channel, customerId, json)
    auditConnector.sendExplicitAudit(messageResponse.auditType, details)
  }

  def auditMissingMovementEvent(request: AuthenticatedRequest[_], arrivalId: ArrivalId): Unit = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)
    val details                    = AuthenticatedAuditDetails(request.channel, request.enrolmentId, Json.obj("arrivalId" -> arrivalId))
    auditConnector.sendExplicitAudit(AuditType.MissingMovementRequested, details)
  }

  def authAudit(auditType: String, details: AuthenticationDetails)(implicit hc: HeaderCarrier): Unit =
    auditConnector.sendExplicitAudit(auditType, details)

}
