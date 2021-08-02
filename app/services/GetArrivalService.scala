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

import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import models.RequestError
import repositories.ArrivalMovementRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class GetArrivalService @Inject()(repository: ArrivalMovementRepository)(implicit ec: ExecutionContext) {

  def getArrivalById(arrivalId: ArrivalId): Future[Either[RequestError, Arrival]] =
    repository.get(arrivalId).map {
      case Some(arrival) => Right(arrival)
      case None          => Left(ArrivalNotFoundError(s"[GetArrivalService][getArrivalById] Unable to retrieve arrival message for arrival id: ${arrivalId.index}"))
    }
}
