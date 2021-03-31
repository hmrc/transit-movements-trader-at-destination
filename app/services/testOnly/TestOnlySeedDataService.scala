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

package services.testOnly

import java.time.Clock
import java.time.LocalDateTime

import cats.data.NonEmptyList
import models.ArrivalStatus.Initialized
import models.MessageType.ArrivalNotification
import models.Arrival
import models.ArrivalId
import models.ChannelType
import models.MessageStatus
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import models.testOnly.SeedDataParameters
import play.api.libs.json.JsObject

import scala.xml.NodeSeq

object TestOnlySeedDataService {

  def seedArrivals(seedDataParameters: SeedDataParameters, clock: Clock): Iterator[Arrival] =
    for {
      arrivalId   <- Iterator.from(999999).map(ArrivalId(_))
      (eori, mrn) <- TestOnlyDataIteratorService.seedDataIterator(seedDataParameters)
    } yield makeArrivalMovement(eori.format, mrn.format, arrivalId, clock)

  private def makeArrivalMovement(eori: String, mrn: String, arrivalId: ArrivalId, clock: Clock): Arrival = {

    val dateTime = LocalDateTime.now(clock)

    val movementMessage = MovementMessageWithStatus(
      dateTime,
      ArrivalNotification,
      NodeSeq.Empty,
      MessageStatus.SubmissionPending,
      1,
      JsObject.empty
    )

    Arrival(
      arrivalId,
      ChannelType.web,
      MovementReferenceNumber(mrn),
      eori,
      Initialized,
      dateTime,
      dateTime,
      dateTime,
      NonEmptyList.one(movementMessage),
      2
    )
  }

}
