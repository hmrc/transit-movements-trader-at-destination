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

import connectors.MessageConnector
import javax.inject.Inject
import play.api.Logger
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.MessageId
import models.MessageStatus
import models.MovementMessageWithState
import models.SubmissionResult
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

  private val logger = Logger(getClass)

  def submitMessage(arrivalId: ArrivalId, messageId: MessageId, message: MovementMessageWithState)(implicit hc: HeaderCarrier): Future[SubmissionResult] =
    arrivalMovementRepository.addNewMessage(arrivalId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionResult.FailureInternal)

      case Success(_) => {
        messageConnector
          .post(arrivalId, message, OffsetDateTime.now)
          .flatMap {
            _ =>
              arrivalMovementRepository
                .setArrivalStateAndMessageState(arrivalId, messageId, ArrivalStatus.ArrivalSubmitted, MessageStatus.SubmissionSucceeded)
                .map {
                  _ =>
                    SubmissionResult.Success
                }
                .recover({
                  case _ =>
                    SubmissionResult.FailureInternal
                })
          }
          .recoverWith {
            case error => {
              logger.error(s"Existing Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

              arrivalMovementRepository.setMessageState(arrivalId, messageId.index, message.status.transition(SubmissionResult.FailureInternal)) map {
                _ =>
                  SubmissionResult.FailureExternal
              }
            }
          }
      }
    }

  def submitArrival(arrival: Arrival)(implicit hc: HeaderCarrier): Future[SubmissionResult] =
    arrivalMovementRepository
      .insert(arrival)
      .flatMap {
        _ =>
          val message   = arrival.messages.head.asInstanceOf[MovementMessageWithState]
          val messageId = new MessageId(arrival.messages.length - 1)

          messageConnector
            .post(arrival.arrivalId, message, OffsetDateTime.now)
            .flatMap {
              _ =>
                arrivalMovementRepository
                  .setArrivalStateAndMessageState(arrival.arrivalId, messageId, ArrivalStatus.ArrivalSubmitted, MessageStatus.SubmissionSucceeded)
                  .map {
                    _ =>
                      SubmissionResult.Success
                  }
                  .recover({
                    case _ =>
                      SubmissionResult.FailureInternal
                  })
            }
            .recoverWith {
              case error =>
                logger.error(s"New Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

                arrivalMovementRepository.setMessageState(arrival.arrivalId, messageId.index, message.status.transition(SubmissionResult.FailureInternal)) map {
                  _ =>
                    SubmissionResult.FailureExternal
                }
            }

      }
      .recover {
        case _ =>
          SubmissionResult.FailureInternal
      }

}
