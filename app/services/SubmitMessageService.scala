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

package services

import java.time.OffsetDateTime

import cats.implicits._
import connectors.MessageConnector
import javax.inject.Inject
import models.Arrival
import models.ArrivalId
import models.ArrivalPutUpdate
import models.ArrivalStatus
import models.ArrivalStatusUpdate
import models.CompoundStatusUpdate
import models.MessageId
import models.MessageStatus
import models.MessageStatusUpdate
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import models.SubmissionProcessingResult
import play.api.Logger
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SubmitMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext) {

  def submitMessage(arrivalId: ArrivalId, messageId: MessageId, message: MovementMessageWithStatus, arrivalStatus: ArrivalStatus)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository.addNewMessage(arrivalId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)

      case Success(_) => {
        messageConnector
          .post(arrivalId, message, OffsetDateTime.now)
          .flatMap {
            _ =>
              arrivalMovementRepository
                .setArrivalStateAndMessageState(arrivalId, messageId, arrivalStatus, MessageStatus.SubmissionSucceeded)
                .map {
                  _ =>
                    SubmissionProcessingResult.SubmissionSuccess
                }
                .recover({
                  case _ =>
                    SubmissionProcessingResult.SubmissionFailureInternal
                })
          }
          .recoverWith {
            case error => {
              Logger.warn(s"Existing Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

              arrivalMovementRepository.setMessageState(arrivalId,
                                                        messageId.index,
                                                        message.status.transition(SubmissionProcessingResult.SubmissionFailureInternal)) map {
                _ =>
                  SubmissionProcessingResult.SubmissionFailureExternal
              }
            }
          }
      }
    }

  def submitIe007Message(arrivalId: ArrivalId, messageId: MessageId, message: MovementMessageWithStatus, mrn: MovementReferenceNumber)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    arrivalMovementRepository.addNewMessage(arrivalId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)

      case Success(_) => {
        messageConnector
          .post(arrivalId, message, OffsetDateTime.now)
          .flatMap {
            _ =>
              arrivalMovementRepository
                .updateArrival(
                  ArrivalPutUpdate.selector(arrivalId),
                  ArrivalPutUpdate(mrn,
                                   CompoundStatusUpdate(ArrivalStatusUpdate(ArrivalStatus.ArrivalSubmitted),
                                                        MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded)))
                )
                .map {
                  _ =>
                    SubmissionProcessingResult.SubmissionSuccess
                }
                .recover({
                  case _ =>
                    SubmissionProcessingResult.SubmissionFailureInternal
                })
          }
          .recoverWith {
            case error => {
              Logger.warn(s"Existing Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

              arrivalMovementRepository
                .updateArrival(
                  ArrivalPutUpdate.selector(arrivalId),
                  MessageStatusUpdate(messageId, message.status.transition(SubmissionProcessingResult.SubmissionFailureInternal))
                )
                .map {
                  _ =>
                    SubmissionProcessingResult.SubmissionFailureExternal
                }
            }
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
            .post(arrival.arrivalId, message, OffsetDateTime.now)
            .flatMap {
              _ =>
                arrivalMovementRepository
                  .setArrivalStateAndMessageState(arrival.arrivalId, messageId, ArrivalStatus.ArrivalSubmitted, MessageStatus.SubmissionSucceeded)
                  .map {
                    _ =>
                      SubmissionProcessingResult.SubmissionSuccess
                  }
                  .recover({
                    case _ =>
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })
            }
            .recoverWith {
              case error =>
                Logger.warn(s"New Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

                arrivalMovementRepository.setMessageState(arrival.arrivalId,
                                                          messageId.index,
                                                          message.status.transition(SubmissionProcessingResult.SubmissionFailureInternal)) map {
                  _ =>
                    SubmissionProcessingResult.SubmissionFailureExternal
                }
            }

      }
      .recover {
        case _ =>
          SubmissionProcessingResult.SubmissionFailureInternal
      }

}
