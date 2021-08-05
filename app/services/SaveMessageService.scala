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
import logging.Logging
import models.ArrivalStatus
import models.ChannelType
import models.InboundMessageResponse
import models.MessageSender
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult._
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq
import models.MessageId

class SaveMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementMessageService,
  xmlValidationService: XmlValidationService,
  auditService: AuditService
)(implicit ec: ExecutionContext)
    extends Logging {

  def validateXmlAndSaveMessage(
    nextMessageId: MessageId,
    messageXml: NodeSeq,
    messageSender: MessageSender,
    messageResponse: InboundMessageResponse,
    arrivalStatus: ArrivalStatus,
    customerId: String,
    channel: ChannelType
  )(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    xmlValidationService.validate(messageXml.toString(), messageResponse.xsdFile) match {
      case Success(_) =>
        arrivalMovementService.makeInboundMessage(nextMessageId, messageSender.messageCorrelationId, messageResponse.messageType)(messageXml) match {
          case Right(message) =>
            arrivalMovementRepository
              .addResponseMessage(messageSender.arrivalId, message, arrivalStatus)
              .map {
                case Success(_) =>
                  logger.debug(s"Saved message successfully")
                  auditService.auditNCTSMessages(channel, customerId, messageResponse, message)
                  SubmissionSuccess
                case Failure(error) =>
                  logger.warn(s"Failed to save message with error: $error")
                  SubmissionFailureInternal
              }
          case Left(error) =>
            logger.warn(s"Failed to create message with error: $error")
            Future.successful(SubmissionFailureExternal)
        }
      case Failure(e) =>
        logger.warn(s"Failure to validate against XSD. Exception: ${e.getMessage}")
        Future.successful(SubmissionFailureExternal)
    }
}
