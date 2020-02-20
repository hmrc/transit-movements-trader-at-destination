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

import javax.inject.Inject
import models.ArrivalMovement
import models.request.InterchangeControlReference
import models.request.MovementReferenceId
import reactivemongo.api.commands.WriteResult
import repositories._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DatabaseServiceImpl @Inject()(sequentialInterchangeControlReferenceIdRepository: SequentialInterchangeControlReferenceIdRepository,
                                    movementReferenceIdRepository: MovementReferenceIdRepository,
                                    arrivalMovementRepository: ArrivalMovementRepository)
    extends DatabaseService {

  def getInterchangeControlReferenceId: Future[Either[FailedCreatingInterchangeControlReference, InterchangeControlReference]] =
    sequentialInterchangeControlReferenceIdRepository
      .nextInterchangeControlReferenceId()
      .map {
        reference =>
          Right(reference)
      }
      .recover {
        case _ =>
          Left(FailedCreatingInterchangeControlReference)
      }

  def getMovementReferenceId: Future[Either[FailedCreatingNextMovementReferenceId, MovementReferenceId]] =
    movementReferenceIdRepository
      .nextId()
      .map {
        movementReferenceId =>
          Right(movementReferenceId)
      }
      .recover {
        case _ => Left(FailedCreatingNextMovementReferenceId)
      }

  def saveArrivalNotification(arrivalMovement: ArrivalMovement): Future[Either[FailedSavingArrivalNotification, WriteResult]] =
    arrivalMovementRepository
      .persistToMongo(arrivalMovement)
      .map {
        writeResult =>
          Right(writeResult)
      }
      .recover {
        case _ =>
          Left(FailedSavingArrivalNotification)
      }

}

trait DatabaseService {
  def getInterchangeControlReferenceId: Future[Either[FailedCreatingInterchangeControlReference, InterchangeControlReference]]
  def saveArrivalNotification(arrivalMovement: ArrivalMovement): Future[Either[FailedSavingArrivalNotification, WriteResult]]
}
