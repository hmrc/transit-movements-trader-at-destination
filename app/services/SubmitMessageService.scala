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
import connectors.MessageConnector.EisSubmissionResult._
import logging.Logging
import models._
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Clock
import java.time.OffsetDateTime
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

  def submitMessage(arrivalId: ArrivalId, messageId: MessageId, message: MovementMessageWithStatus, channelType: ChannelType)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository.addNewMessage(arrivalId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)

      case Success(_) =>
        messageConnector
          .post(arrivalId, message, OffsetDateTime.now, channelType)
          .flatMap {
            submissionResult =>
              submissionResult match {
                case EisSubmissionSuccessful =>
                  val newStatus = message.status.transition(submissionResult)

                  updateArrivalAndMessage(arrivalId, messageId, newStatus)
                    .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                    .recover({
                      case NonFatal(e) =>
                        // TODO: Can this recover be moved to the repository layer.
                        //  Encode the exception in the failed Future that Reactive Mongo returns as an ADT
                        logger.error("Mongo failure when updating message status", e)
                        SubmissionProcessingResult.SubmissionFailureInternal
                    }) // TODO: Should be success?

                case submissionResult: EisSubmissionRejected =>
                  logger.warn(s"Failure for submitMessage of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  updateMessage(arrivalId, message, messageId, submissionResult)
                    .map(_ =>
                      submissionResult match {
                        case ErrorInPayload =>
                          SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                        case VirusFoundOrInvalidToken =>
                          SubmissionProcessingResult.SubmissionFailureInternal
                    })
                    .recover({
                      case NonFatal(e) =>
                        logger.error("Mongo failure when updating message status", e)
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionFailureDownstream =>
                  logger.warn(s"Failure for submitMessage of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  updateMessage(arrivalId, message, messageId, submissionResult)
                    .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                    .recover({
                      case NonFatal(e) =>
                        logger.error("Mongo failure when updating message status", e)
                        SubmissionProcessingResult.SubmissionFailureExternal
                    })
              }
          }
    }

  def submitIe007Message(arrivalId: ArrivalId,
                         messageId: MessageId,
                         message: MovementMessageWithStatus,
                         mrn: MovementReferenceNumber,
                         channelType: ChannelType)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository.addNewMessage(arrivalId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)

      case Success(_) =>
        messageConnector
          .post(arrivalId, message, OffsetDateTime.now, channelType)
          .flatMap {
            submissionResult =>
              submissionResult match {
                case EisSubmissionSuccessful =>
                  val selector  = ArrivalIdSelector(arrivalId)
                  val newStatus = message.status.transition(submissionResult)
                  val update = ArrivalPutUpdate(
                    mrn,
                    MessageStatusUpdate(messageId, newStatus)
                  )

                  arrivalMovementRepository
                    .updateArrival(selector, update)
                    .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                    .recover({
                      case NonFatal(e) =>
                        logger.error("Mongo failure when updating message status", e)
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionRejected =>
                  logger.warn(s"Failure for submitIe007Message of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  updateMessage(arrivalId, message, messageId, submissionResult)
                    .map(_ =>
                      submissionResult match {
                        case ErrorInPayload =>
                          SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                        case VirusFoundOrInvalidToken =>
                          SubmissionProcessingResult.SubmissionFailureInternal
                    })
                    .recover({
                      case NonFatal(e) =>
                        logger.error("Mongo failure when updating message status", e)
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionFailureDownstream =>
                  logger.warn(s"Failure for submitIe007Message of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  updateMessage(arrivalId, message, messageId, submissionResult)
                    .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                    .recover({
                      case NonFatal(e) =>
                        logger.error("Mongo failure when updating message status", e)
                        SubmissionProcessingResult.SubmissionFailureExternal
                    })
              }
          }
    }

  def submitArrival(arrival: Arrival)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository
      .insert(arrival)
      .flatMap {
        _ =>
          val (message, messageId) = arrival.messagesWithId.head.leftMap(_.asInstanceOf[MovementMessageWithStatus])

          messageConnector
            .post(arrival.arrivalId, message, OffsetDateTime.now, arrival.channel)
            .flatMap {
              case EisSubmissionSuccessful =>
                updateArrivalAndMessage(arrival.arrivalId, messageId)
                  .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                  .recover({
                    case NonFatal(e) =>
                      logger.error("Mongo failure when updating message status", e)
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })

              case submissionResult: EisSubmissionRejected =>
                logger.warn(s"Failure for submitArrival of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                updateMessage(arrival.arrivalId, message, messageId, submissionResult)
                  .map(_ =>
                    submissionResult match {
                      case ErrorInPayload =>
                        SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                      case VirusFoundOrInvalidToken =>
                        SubmissionProcessingResult.SubmissionFailureInternal
                  })
                  .recover({
                    case NonFatal(e) =>
                      logger.error("Mongo failure when updating message status", e)
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })

              case submissionResult: EisSubmissionFailureDownstream =>
                logger.warn(s"Failure for submitArrival of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                updateMessage(arrival.arrivalId, message, messageId, submissionResult)
                  .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                  .recover({
                    case NonFatal(e) =>
                      logger.error("Mongo failure when updating message status", e)
                      SubmissionProcessingResult.SubmissionFailureExternal
                  })
            }

      }
      .recover {
        case NonFatal(e) =>
          logger.error("Mongo failure when inserting a new arrival", e)
          SubmissionProcessingResult.SubmissionFailureInternal
      }

  private def updateMessage(
    arrivalId: ArrivalId,
    message: MovementMessageWithStatus,
    messageId: MessageId,
    submissionResult: EisSubmissionResult
  ): Future[Try[Unit]] = {
    val selector = MessageSelector(arrivalId, messageId)
    val modifier = MessageStatusUpdate(messageId, message.status.transition(submissionResult))

    arrivalMovementRepository.updateArrival(selector, modifier)
  }

  private def updateArrivalAndMessage(
    arrivalId: ArrivalId,
    messageId: MessageId,
    messageState: MessageStatus = MessageStatus.SubmissionSucceeded
  ): Future[Try[Unit]] = {
    val selector = ArrivalIdSelector(arrivalId)
    val modifier = MessageStatusUpdate(messageId, messageState)

    arrivalMovementRepository.updateArrival(selector, modifier)
  }

}
