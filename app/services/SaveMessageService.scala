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

package services

import audit.AuditService
import com.google.inject.Inject
import com.kenshoo.play.metrics.Metrics
import logging.Logging
import metrics.HasActionMetrics
import metrics.Monitors
import models.SubmissionProcessingResult._
import models.FailedToCreateMessage
import models.FailedToSaveMessage
import models.FailedToValidateMessage
import models.InboundMessageRequest
import models.MessageSender
import models.SubmissionState
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class SaveMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementMessageService,
  xmlValidationService: XmlValidationService,
  auditService: AuditService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends Logging {

  def validateXmlAndSaveMessage(
    inboundRequest: InboundMessageRequest,
    xml: NodeSeq,
    messageSender: MessageSender
  )(implicit hc: HeaderCarrier): Future[Either[SubmissionState, Unit]] =
    inboundRequest match {
      case InboundMessageRequest(arrival, nextStatus, inboundMessageResponse) =>
        xmlValidationService.validate(xml.toString(), inboundMessageResponse.xsdFile) match {
          case Success(_) =>
            arrivalMovementService.makeInboundMessage(arrival.nextMessageId, messageSender.messageCorrelationId, inboundMessageResponse.messageType)(xml) match {
              case Right(message) =>
                arrivalMovementRepository
                  .addResponseMessage(messageSender.arrivalId, message, nextStatus)
                  .map {
                    case Success(_) =>
                      logger.debug(s"Saved message successfully")
                      auditService.auditNCTSMessages(arrival.channel, inboundRequest.inboundMessageResponse, message)
                      Right(())
                    case Failure(error) =>
                      Left(FailedToSaveMessage(s"[SaveMessageService][validateXmlAndSaveMessage] Failed to save message with error: $error"))
                  }
              case Left(error) =>
                Future.successful(Left(FailedToCreateMessage(s"[SaveMessageService][validateXmlAndSaveMessage] Failed to create message with error: $error")))
            }
          case Failure(e) =>
            Future.successful(
              Left(FailedToValidateMessage(s"[SaveMessageService][validateXmlAndSaveMessage] Failure to validate against XSD. Exception: ${e.getMessage}")))
        }
    }
}
