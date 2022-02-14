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

package services

import cats.implicits._
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult.DownstreamGatewayTimeout
import connectors.MessageConnector.EisSubmissionResult._
import logging.Logging
import models._
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Clock
import java.time.OffsetDateTime
import java.util.concurrent.TimeoutException
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

class SubmitMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  messageConnector: MessageConnector,
)(implicit clock: Clock, ec: ExecutionContext)
    extends Logging {

  def submitMessage(arrivalId: ArrivalId, message: MovementMessageWithStatus, channelType: ChannelType)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository
      .addNewMessage(arrivalId, message)
      .flatMap {
        case Failure(_) =>
          Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)
        case Success(_) =>
          val modifier = MessageStatusUpdate(message.messageId, MessageStatus.SubmissionSucceeded)
          submitToEis(arrivalId, message, channelType, modifier)("submitMessage")
      }

  def submitIe007Message(arrivalId: ArrivalId, message: MovementMessageWithStatus, mrn: MovementReferenceNumber, channelType: ChannelType)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository
      .addNewMessage(arrivalId, message)
      .flatMap {
        case Failure(_) =>
          Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)
        case Success(_) =>
          val modifier = ArrivalPutUpdate(
            mrn,
            MessageStatusUpdate(message.messageId, MessageStatus.SubmissionSucceeded)
          )
          submitToEis(arrivalId, message, channelType, modifier)("submitIe007Message")
      }

  def submitArrival(arrival: Arrival)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository
      .insert(arrival)
      .flatMap {
        _ =>
          val (message, _) = arrival.messagesWithId.head.leftMap(_.asInstanceOf[MovementMessageWithStatus])
          val modifier     = MessageStatusUpdate(message.messageId, MessageStatus.SubmissionSucceeded)
          submitToEis(arrival.arrivalId, message, arrival.channel, modifier)("submitArrival")
      }
      .recover {
        case NonFatal(e) =>
          logger.error("Mongo failure when inserting a new arrival", e)
          SubmissionProcessingResult.SubmissionFailureInternal
      }

  private def submitToEis(
    arrivalId: ArrivalId,
    message: MovementMessageWithStatus,
    channel: ChannelType,
    modifier: ArrivalUpdate
  )(method: String)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    messageConnector
      .post(arrivalId, message, OffsetDateTime.now, channel)
      .flatMap {
        case EisSubmissionSuccessful =>
          updateArrivalAndMessage(arrivalId, modifier)

        case submissionResult: EisSubmissionRejected =>
          logger.warn(s"Failure for $method of type: ${message.messageType.code}, and details: ${submissionResult.toString}")
          updateMessage(arrivalId, message, submissionResult)(_ =>
            submissionResult match {
              case ErrorInPayload =>
                SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
              case VirusFoundOrInvalidToken =>
                SubmissionProcessingResult.SubmissionFailureInternal
          })(SubmissionProcessingResult.SubmissionFailureInternal)

        case submissionResult: EisSubmissionFailureDownstream =>
          logger.warn(s"Failure for $method of type: ${message.messageType.code}, and details: ${submissionResult.toString}")
          updateMessage(arrivalId, message, submissionResult)(
            _ => SubmissionProcessingResult.SubmissionFailureExternal
          )(SubmissionProcessingResult.SubmissionFailureExternal)
      }
      .recoverWith {
        case e: TimeoutException =>
          logger.error("Submission to EIS timed out", e)
          updateMessage(arrivalId, message, DownstreamGatewayTimeout)(
            _ => SubmissionProcessingResult.SubmissionFailureExternal
          )(SubmissionProcessingResult.SubmissionFailureExternal)
      }

  private def updateMessage(
    arrivalId: ArrivalId,
    message: MovementMessageWithStatus,
    submissionResult: EisSubmissionResult
  )(processResult: Try[Unit] => SubmissionProcessingResult)(defaultResult: SubmissionProcessingResult): Future[SubmissionProcessingResult] = {
    val selector = MessageSelector(arrivalId, message.messageId)
    val modifier = MessageStatusUpdate(message.messageId, message.status.transition(submissionResult))
    updateArrival(selector, modifier)(processResult)(defaultResult)
  }

  private def updateArrivalAndMessage(
    arrivalId: ArrivalId,
    modifier: ArrivalUpdate
  ): Future[SubmissionProcessingResult] = {
    val selector = ArrivalIdSelector(arrivalId)
    updateArrival(selector, modifier)(
      _ => SubmissionProcessingResult.SubmissionSuccess
    )(SubmissionProcessingResult.SubmissionFailureInternal)
  }

  private def updateArrival(
    selector: ArrivalSelector,
    modifier: ArrivalUpdate
  )(processResult: Try[Unit] => SubmissionProcessingResult)(defaultResult: SubmissionProcessingResult): Future[SubmissionProcessingResult] =
    arrivalMovementRepository
      .updateArrival(selector, modifier)
      .map(processResult)
      .recover({
        case NonFatal(e) =>
          logger.error("Mongo failure when updating message status", e)
          defaultResult
      })

}
