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
import cats.data.EitherT
import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import models.ArrivalWithoutMessages
import models.MessageResponse
import models.MovementMessage
import models.SubmissionState
import models.ChannelType.deleted
import repositories.ArrivalMovementRepository
import uk.gov.hmrc.http.HeaderCarrier
import javax.inject.Inject

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class GetArrivalService @Inject()(repository: ArrivalMovementRepository, auditService: AuditService)(implicit ec: ExecutionContext) {

  def getArrivalById(arrivalId: ArrivalId): Future[Either[SubmissionState, Arrival]] =
    repository.get(arrivalId).map {
      case Some(arrival) => Right(arrival)
      case None          => Left(ArrivalNotFoundError(s"[GetArrivalService][getArrivalById] Unable to retrieve arrival movement for arrival id: ${arrivalId.index}"))
    }

  def getArrivalWithoutMessagesById(arrivalId: ArrivalId): Future[Either[SubmissionState, ArrivalWithoutMessages]] =
    repository.getWithoutMessages(arrivalId).map {
      case Some(arrivalWithoutMessages) => Right(arrivalWithoutMessages)
      case None =>
        Left(ArrivalNotFoundError(s"[GetArrivalService][getArrivalWithoutMessagesById] Unable to retrieve arrival movement for arrival id: ${arrivalId.index}"))
    }

  def getArrivalAndAudit(arrivalId: ArrivalId, messageResponse: MessageResponse, movementMessage: MovementMessage)(
    implicit hc: HeaderCarrier): EitherT[Future, SubmissionState, ArrivalWithoutMessages] =
    EitherT(
      getArrivalWithoutMessagesById(arrivalId).map {
        _.left.map {
          submissionState =>
            auditService.auditNCTSMessages(deleted, "deleted", messageResponse, movementMessage)
            submissionState
        }
      }
    )
}
