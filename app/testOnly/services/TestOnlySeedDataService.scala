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

package testOnly.services

import cats.data.NonEmptyList
import models.ArrivalStatus.Initialized
import models.MessageType.ArrivalNotification
import models.Arrival
import models.ArrivalId
import models.ChannelType
import models.MessageStatus
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import testOnly.models.SeedDataParameters

import java.time.Clock
import java.time.LocalDateTime
import javax.inject.Inject
import scala.xml.Elem

private[testOnly] class TestOnlySeedDataService @Inject()(testDataGenerator: TestDataGenerator) {

  def seedArrivals(seedDataParameters: SeedDataParameters, clock: Clock): Iterator[Arrival] =
    seedDataParameters.seedData
      .map {
        case (id, eori, mrn) =>
          testDataGenerator.arrivalMovement(eori, mrn, id, seedDataParameters.channelType)
      }

}
