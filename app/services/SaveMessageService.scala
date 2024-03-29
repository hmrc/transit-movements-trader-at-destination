/*
 * Copyright 2023 HM Revenue & Customs
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
import models.FailedToSaveMessage
import models.InboundMessageRequest
import models.MessageSender
import models.SubmissionState
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SaveMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  auditService: AuditService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends Logging {

  def saveInboundMessage(
    inboundRequest: InboundMessageRequest,
    messageSender: MessageSender
  )(implicit hc: HeaderCarrier): Future[Either[SubmissionState, Unit]] =
    inboundRequest match {
      case InboundMessageRequest(arrival, inboundMessageResponse, movementMessage) =>
        arrivalMovementRepository
          .addResponseMessage(messageSender.arrivalId, movementMessage)
          .map {
            case Success(_) =>
              logger.debug(s"Saved message successfully")
              auditService.auditNCTSMessages(arrival.channel, arrival.eoriNumber, inboundMessageResponse, movementMessage)
              Right(())
            case Failure(error) =>
              Left(FailedToSaveMessage(s"[SaveMessageService][validateXmlAndSaveMessage] Failed to save message with error: $error"))
          }
    }

}
