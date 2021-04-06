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
import play.api.libs.json._
import play.api.libs.functional.syntax._

class SeedDataParameters(
  val numberOfUsers: Int,
  val movementsPerUser: Int,
  val startArrivalId: ArrivalId,
  firstEoriValue: Option[SeedEori] = None
) {

  override def equals(obj: Any): Boolean = obj match {
    case x: SeedDataParameters => (numberOfUsers == x.numberOfUsers) && (movementsPerUser == x.movementsPerUser)
    case _                     => false
  }

  val startEori: SeedEori = firstEoriValue.getOrElse(SeedEori("ZZ", 1, 12))
  val startMrn: SeedMrn   = SeedMrn("21GB", 1, 14)
}

object SeedDataParameters {

  def apply(numberOfUsers: Int, movementsPerUser: Int, startArrivalId: ArrivalId, firstEoriValue: Option[SeedEori]): SeedDataParameters =
    new SeedDataParameters(numberOfUsers, movementsPerUser, startArrivalId, firstEoriValue)

  def unapply(seedDataParameters: SeedDataParameters): Option[(SeedEori, Int, SeedMrn, Int, ArrivalId)] =
    Some(
      (
        seedDataParameters.startEori,
        seedDataParameters.numberOfUsers,
        seedDataParameters.startMrn,
        seedDataParameters.movementsPerUser,
        seedDataParameters.startArrivalId
      )
    )

  implicit val reads: Reads[SeedDataParameters] =
    (
      (__ \ "numberOfUsers").read[Int] and
        (__ \ "movementsPerUser").read[Int] and
        (__ \ "startArrivalId").read[ArrivalId] and
        (__ \ "startEori").readNullable[SeedEori]
    )(SeedDataParameters.apply _)
}
