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
import models.ArrivalId
import models.ArrivalState
import models.MessageState
import models.MovementMessage
import models.MovementMessageWithState
import models.SubmissionResult
import models.ArrivalState.ArrivalSubmitted
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpErrorFunctions

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SubmitMessageService @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends HttpErrorFunctions {

  def submit(arrivalId: ArrivalId, messageId: Int, message: MovementMessageWithState)(implicit hc: HeaderCarrier): Future[SubmissionResult] =
    arrivalMovementRepository.addNewMessage(arrivalId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionResult.Failure)

      case Success(_) =>
        (for {
          _ <- messageConnector.post(arrivalId, message, OffsetDateTime.now)
          x <- arrivalMovementRepository.setState(arrivalId, ArrivalState.ArrivalSubmitted)
          y <- arrivalMovementRepository.setMessageState(arrivalId, messageId, message.state.transition(SubmissionResult.Success))
        } yield {
          SubmissionResult.Success
        }).recoverWith {
          case _ =>
            arrivalMovementRepository.setMessageState(arrivalId, messageId, message.state.transition(SubmissionResult.Failure)) map {
              _ =>
                SubmissionResult.Failure
            }
        }
    }

}
