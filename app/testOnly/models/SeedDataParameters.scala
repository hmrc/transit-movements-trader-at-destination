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
import models.ChannelType
import play.api.libs.functional.syntax._
import play.api.libs.json._

private[testOnly] class SeedDataParameters(
  val numberOfUsers: Int,
  val movementsPerUser: Int,
  val startArrivalId: ArrivalId,
  firstEoriValue: Option[SeedEori] = None,
  channel: Option[ChannelType]
) {

  override def equals(obj: Any): Boolean = obj match {
    case x: SeedDataParameters => (numberOfUsers == x.numberOfUsers) && (movementsPerUser == x.movementsPerUser)
    case _                     => false
  }

  val startEori: SeedEori = firstEoriValue.getOrElse(SeedEori("ZZ", 1, 12))

  val startMrn: SeedMrn = SeedMrn("21GB", 1, 14)

  val numberOfMovements: Int = numberOfUsers * movementsPerUser

  val channelType: ChannelType = channel.getOrElse(ChannelType.web)

  private val arrivalIdIterator: Iterator[ArrivalId] =
    (startArrivalId.index to (startArrivalId.index + numberOfMovements)).iterator
      .map(ArrivalId(_))

  val seedData: Iterator[(ArrivalId, SeedEori, SeedMrn)] = {

    val eoriMrnIterator =
      (startEori.suffix to (startEori.suffix + numberOfUsers - 1)).toIterator
        .map(SeedEori(startEori.prefix, _, startEori.padLength))
        .flatMap {
          eori =>
            (startMrn.suffix to (startMrn.suffix + movementsPerUser - 1)).toIterator
              .map(SeedMrn(startMrn.prefix, _, startMrn.padLength))
              .map(mrn => (eori, mrn))
        }

    arrivalIdIterator.zip(eoriMrnIterator).map {
      case (arrivalId, (eori, mrn)) => (arrivalId, eori, mrn)
    }
  }

}

object SeedDataParameters {

  def apply(numberOfUsers: Int,
            movementsPerUser: Int,
            startArrivalId: ArrivalId,
            firstEoriValue: Option[SeedEori],
            channel: Option[ChannelType]): SeedDataParameters =
    new SeedDataParameters(numberOfUsers, movementsPerUser, startArrivalId, firstEoriValue, channel)

  implicit val reads: Reads[SeedDataParameters] =
    (
      (__ \ "numberOfUsers").read[Int] and
        (__ \ "movementsPerUser").read[Int] and
        (__ \ "startArrivalId").read[ArrivalId] and
        (__ \ "startEori").readNullable[SeedEori] and
        (__ \ "channel").readNullable[ChannelType]
    )(SeedDataParameters.apply _)
}
