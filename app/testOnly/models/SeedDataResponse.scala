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

package testOnly.models

import models.ArrivalId
import play.api.libs.json.Json

case class SeedDataResponse(
  numberOfUsers: Int,
  eoriRangeStart: SeedEori,
  eoriRangeEnd: SeedEori,
  movementsPerUser: Int,
  mrnRangeStart: SeedMrn,
  mrnRangeEnd: SeedMrn,
  totalInsertedMovements: Int,
  arrivalIdRangeStart: ArrivalId,
  arrivalIdRangeEnd: ArrivalId
)

object SeedDataResponse {

  def apply(seedDataParameters: SeedDataParameters): SeedDataResponse = {
    val startEori        = seedDataParameters.startEori
    val numberOfUsers    = seedDataParameters.numberOfUsers
    val startMrn         = seedDataParameters.startMrn
    val movementsPerUser = seedDataParameters.movementsPerUser
    val startArrivalId   = seedDataParameters.startArrivalId

    val maxEori: SeedEori = SeedEori(startEori.prefix, startEori.suffix + numberOfUsers, startEori.padLength)
    val maxMrn: SeedMrn   = SeedMrn(startMrn.prefix, startMrn.suffix + movementsPerUser, startMrn.padLength)

    val endArrivalId = ArrivalId(startArrivalId.index + seedDataParameters.numberOfMovements - 1)

    SeedDataResponse(
      numberOfUsers = numberOfUsers,
      eoriRangeStart = startEori,
      eoriRangeEnd = maxEori,
      movementsPerUser = movementsPerUser,
      mrnRangeStart = startMrn,
      mrnRangeEnd = maxMrn,
      totalInsertedMovements = seedDataParameters.numberOfMovements,
      arrivalIdRangeStart = startArrivalId,
      arrivalIdRangeEnd = endArrivalId
    )
  }

  implicit val format = Json.format[SeedDataResponse]
}
