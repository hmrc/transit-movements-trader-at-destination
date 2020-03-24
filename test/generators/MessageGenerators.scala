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
import java.time.LocalTime

import models.messages._
import models.Arrival
import models.ArrivalMovement
import models.MessageType
import models.MovementMessage
import models.RejectionError
import models.State
import models.TimeStampedMessageJson
import models.request
import models.request._
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

  implicit lazy val arbitraryMessageJson: Arbitrary[TimeStampedMessageJson] = {
    Arbitrary {
      for {
        date    <- datesBetween(pastDate, dateNow)
        time    <- timesBetween(pastDate, dateNow)
        message <- arbitrary[ArrivalNotificationMessage]

      } yield TimeStampedMessageJson(date, time, message)
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
        messages                <- seqWithMaxLength[MovementMessage](2)
      } yield
        Arrival(
          arrivalId = arrivalId,
          movementReferenceNumber = movementReferenceNumber.toString,
          eoriNumber = eoriNumber,
          state = state,
          messages = messages
        )
    }
  }

  implicit lazy val arbitraryArrivalMovement: Arbitrary[ArrivalMovement] = {
    Arbitrary {
      for {
        movementReferenceId     <- arbitrary[Int]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        eoriNumber              <- arbitrary[String]
        messages                <- seqWithMaxLength[TimeStampedMessageJson](2)
      } yield
        ArrivalMovement(
          internalReferenceId = movementReferenceId,
          movementReferenceNumber = movementReferenceNumber.toString,
          eoriNumber = eoriNumber,
          messages = messages
        )
    }
  }

  implicit lazy val arbitraryMovementReferenceId: Arbitrary[InternalReferenceId] = {
    Arbitrary {
      for {
        id <- arbitrary[Int]
      } yield InternalReferenceId(id)
    }
  }

  implicit lazy val arbitraryGoodsReleaseNotification: Arbitrary[GoodsReleaseNotificationMessage] =
    Arbitrary {

      for {
        mrn                <- arbitrary[MovementReferenceNumber].map(_.toString())
        releaseDate        <- datesBetween(pastDate, dateNow)
        trader             <- arbitrary[Trader]
        presentationOffice <- stringsWithMaxLength(GoodsReleaseNotificationMessage.Constants.presentationOfficeLength)
      } yield models.messages.GoodsReleaseNotificationMessage(mrn, releaseDate, trader, presentationOffice)
    }

  implicit lazy val arbitraryNormalNotification: Arbitrary[NormalNotificationMessage] =
    Arbitrary {

      for {
        mrn                <- arbitrary[MovementReferenceNumber].map(_.toString())
        place              <- stringsWithMaxLength(NormalNotificationMessage.Constants.notificationPlaceLength)
        date               <- datesBetween(pastDate, dateNow)
        subPlace           <- Gen.option(stringsWithMaxLength(NormalNotificationMessage.Constants.customsSubPlaceLength))
        trader             <- arbitrary[Trader]
        presentationOffice <- stringsWithMaxLength(NormalNotificationMessage.Constants.presentationOfficeLength)
        events             <- Gen.option(seqWithMaxLength[EnRouteEvent](2))
      } yield models.messages.NormalNotificationMessage(mrn, place, date, subPlace, trader, presentationOffice, events)
    }

  implicit lazy val arbitrarySimplifiedNotification: Arbitrary[SimplifiedNotificationMessage] =
    Arbitrary {

      for {
        mrn                <- arbitrary[MovementReferenceNumber].map(_.toString())
        place              <- stringsWithMaxLength(SimplifiedNotificationMessage.Constants.notificationPlaceLength)
        date               <- datesBetween(pastDate, dateNow)
        approvedLocation   <- Gen.option(stringsWithMaxLength(SimplifiedNotificationMessage.Constants.approvedLocationLength))
        trader             <- arbitrary[Trader]
        presentationOffice <- stringsWithMaxLength(SimplifiedNotificationMessage.Constants.presentationOfficeLength)
        events             <- Gen.option(seqWithMaxLength[EnRouteEvent](SimplifiedNotificationMessage.Constants.maxNumberOfEnRouteEvents))
      } yield models.messages.SimplifiedNotificationMessage(mrn, place, date, approvedLocation, trader, presentationOffice, events)
    }

  implicit lazy val arbitraryArrivalNotification: Arbitrary[ArrivalNotificationMessage] =
    Arbitrary {

      Gen.oneOf(arbitrary[NormalNotificationMessage], arbitrary[SimplifiedNotificationMessage])
    }
}
