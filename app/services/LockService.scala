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

import logging.Logging
import models.ArrivalId
import models.DocumentExistsError
import models.FailedToLock
import models.FailedToUnlock
import models.SubmissionState
import repositories.LockRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class LockService @Inject()(lockRepository: LockRepository)(implicit ec: ExecutionContext) extends Logging {

  def lock(arrivalId: ArrivalId): Future[Either[SubmissionState, Unit]] =
    lockRepository
      .lock(arrivalId)
      .flatMap {
        case true => Future.successful(Right(()))
        case false =>
          Future.successful(
            Left(DocumentExistsError(s"[LockService][lock] Could not add lock as document already exists for arrival id $arrivalId"))
          )
      }
      .recoverWith {
        case e: Exception =>
          lockRepository.unlock(arrivalId).map {
            _ =>
              logger.error(s"Failed to lock record", e)
              Left(FailedToLock(s"[LockService][lock] Could not lock for arrival id $arrivalId"))
          }
      }

  def unlock(arrivalId: ArrivalId): Future[Either[SubmissionState, Unit]] =
    lockRepository
      .unlock(arrivalId)
      .map {
        _ =>
          Right(())
      }
      .recoverWith {
        case e: Exception =>
          logger.error(s"Failed to unlock record", e)
          Future(Left(FailedToUnlock(s"[LockService][unlock] Could not unlock for arrival id $arrivalId")))
      }

}
