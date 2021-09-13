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

import cats.implicits._
import connectors.MessageConnector
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

class SubmitMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  messageConnector: MessageConnector,
)(implicit clock: Clock, ec: ExecutionContext)
    extends Logging {

  def submitMessage(arrivalId: ArrivalId, messageId: MessageId, message: MovementMessageWithStatus, arrivalStatus: ArrivalStatus, channelType: ChannelType)(
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

                  arrivalMovementRepository
                    .setArrivalStateAndMessageState(arrivalId, messageId, arrivalStatus, newStatus)
                    .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                    .recover({
                      case _ =>
                        // TODO: Can this recove be moved to the repository layer.
                        //  Encode the exception in the failed Future that Reactive Mongo returns as an ADT
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureInternal
                    }) // TODO: Should be success?

                case submissionResult: EisSubmissionRejected =>
                  logger.warn(s"Failure for submitMessage of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  val messageSelector     = MessageSelector(arrivalId, messageId)
                  val newStatus           = message.status.transition(submissionResult)
                  val messageStatusUpdate = MessageStatusUpdate(messageId, newStatus)

                  arrivalMovementRepository
                    .updateArrival(messageSelector, messageStatusUpdate)
                    .map(_ =>
                      submissionResult match {
                        case ErrorInPayload =>
                          SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                        case VirusFoundOrInvalidToken =>
                          SubmissionProcessingResult.SubmissionFailureInternal
                    })
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionFailureDownstream =>
                  logger.warn(s"Failure for submitMessage of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  val messageSelector     = MessageSelector(arrivalId, messageId)
                  val newStatus           = message.status.transition(submissionResult)
                  val messageStatusUpdate = MessageStatusUpdate(messageId, newStatus)

                  arrivalMovementRepository
                    .updateArrival(messageSelector, messageStatusUpdate)
                    .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
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
                    CompoundStatusUpdate(ArrivalStatusUpdate(ArrivalStatus.ArrivalSubmitted), MessageStatusUpdate(messageId, newStatus)) // TODO: We should use arrival.status.transition here also
                  )

                  arrivalMovementRepository
                    .updateArrival(selector, update)
                    .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionRejected =>
                  logger.warn(s"Failure for submitIe007Message of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  val messageSelector     = MessageSelector(arrivalId, messageId)
                  val newStatus           = message.status.transition(submissionResult)
                  val messageStatusUpdate = MessageStatusUpdate(messageId, newStatus)

                  arrivalMovementRepository
                    .updateArrival(messageSelector, messageStatusUpdate)
                    .map(_ =>
                      submissionResult match {
                        case ErrorInPayload =>
                          SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                        case VirusFoundOrInvalidToken =>
                          SubmissionProcessingResult.SubmissionFailureInternal
                    })
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionFailureDownstream =>
                  logger.warn(s"Failure for submitIe007Message of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  val messageSelector     = MessageSelector(arrivalId, messageId)
                  val newStatus           = message.status.transition(submissionResult)
                  val messageStatusUpdate = MessageStatusUpdate(messageId, newStatus)

                  arrivalMovementRepository
                    .updateArrival(messageSelector, messageStatusUpdate)
                    .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
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
                arrivalMovementRepository
                  .setArrivalStateAndMessageState(arrival.arrivalId, messageId, ArrivalStatus.ArrivalSubmitted, MessageStatus.SubmissionSucceeded)
                  .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                  .recover({
                    case _ =>
                      logger.warn("Mongo failure when updating message status")
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })

              case submissionResult: EisSubmissionRejected =>
                logger.warn(s"Failure for submitArrival of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                val messageSelector     = MessageSelector(arrival.arrivalId, messageId)
                val newStatus           = message.status.transition(submissionResult)
                val messageStatusUpdate = MessageStatusUpdate(messageId, newStatus)

                arrivalMovementRepository
                  .updateArrival(messageSelector, messageStatusUpdate)
                  .map(_ =>
                    submissionResult match {
                      case ErrorInPayload =>
                        SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                      case VirusFoundOrInvalidToken =>
                        SubmissionProcessingResult.SubmissionFailureInternal
                  })
                  .recover({
                    case _ =>
                      logger.warn("Mongo failure when updating message status")
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })

              case submissionResult: EisSubmissionFailureDownstream =>
                logger.warn(s"Failure for submitArrival of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                val messageSelector     = MessageSelector(arrival.arrivalId, messageId)
                val newStatus           = message.status.transition(submissionResult)
                val messageStatusUpdate = MessageStatusUpdate(messageId, newStatus)

                arrivalMovementRepository
                  .updateArrival(messageSelector, messageStatusUpdate)
                  .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                  .recover({
                    case _ =>
                      logger.warn("Mongo failure when updating message status")
                      SubmissionProcessingResult.SubmissionFailureExternal
                  })
            }

      }
      .recover {
        case _ =>
          logger.warn("Mongo failure when inserting a new arrival")
          SubmissionProcessingResult.SubmissionFailureInternal
      }

}
