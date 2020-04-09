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

package generators

import java.time.LocalDate
import java.time.LocalDateTime

import models.Arrival
import models.MessageType
import models.MovementMessage
import models.MovementReferenceNumber
import models.State
import models.request.ArrivalId
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait MessageGenerators extends ModelGenerators {

  private val pastDate: LocalDate = LocalDate.of(1900, 1, 1)
  private val dateNow: LocalDate  = LocalDate.now

  implicit lazy val arbitraryMessageXml: Arbitrary[MovementMessage] = {
    Arbitrary {
      for {
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>) // TODO: revisit this
        messageType <- Gen.oneOf(MessageType.values)
      } yield MovementMessage(date, time, messageType, xml)
    }
  }

  implicit lazy val arbitraryArrivalId: Arbitrary[ArrivalId] = {
    Arbitrary {
      for {
        id <- arbitrary[Int]
      } yield ArrivalId(id)
    }
  }

  implicit lazy val arbitraryState: Arbitrary[State] =
    Arbitrary {
      Gen.oneOf(State.values)
    }

  implicit lazy val arbitraryArrival: Arbitrary[Arrival] = {
    Arbitrary {
      for {
        arrivalId               <- arbitrary[ArrivalId]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        eoriNumber              <- arbitrary[String]
        state                   <- arbitrary[State]
        created                 <- arbitrary[LocalDateTime]
        updated                 <- arbitrary[LocalDateTime]
        messages                <- seqWithMaxLength[MovementMessage](2)
      } yield
        Arrival(
          arrivalId = arrivalId,
          movementReferenceNumber = movementReferenceNumber,
          eoriNumber = eoriNumber,
          state = state,
          created = created,
          updated = updated,
          messages = messages
        )
    }
  }
}
