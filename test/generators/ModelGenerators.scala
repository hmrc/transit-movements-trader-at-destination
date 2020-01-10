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

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset

import models._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait ModelGenerators {

  def dateTimesBetween(min: LocalDateTime, max: LocalDateTime): Gen[LocalDateTime] = {

    def toMillis(date: LocalDateTime): Long =
      date.atZone(ZoneOffset.UTC).toInstant.toEpochMilli

    Gen.choose(toMillis(min), toMillis(max)).map {
      millis =>
        Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC).toLocalDateTime
    }
  }

  def datesBetween(min: LocalDate, max: LocalDate): Gen[LocalDate] = {

    def toMillis(date: LocalDate): Long =
      date.atStartOfDay.atZone(ZoneOffset.UTC).toInstant.toEpochMilli

    Gen.choose(toMillis(min), toMillis(max)).map {
      millis =>
        Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC).toLocalDate
    }
  }

  def stringsWithMaxLength(maxLength: Int): Gen[String] =
    for {
      length <- choose(1, maxLength)
      chars  <- listOfN(length, arbitrary[Char])
    } yield chars.mkString

  def seqWithMaxLength[A](maxLength: Int)(implicit a: Arbitrary[A]): Gen[Seq[A]] =
    for {
      length <- choose(1, maxLength)
      seq    <- listOfN(length, arbitrary[A])
    } yield seq

  implicit lazy val arbitraryMovementReferenceNumber: Arbitrary[MovementReferenceNumber] =
    Arbitrary {
      for {
        year    <- Gen.choose(0, 99).map(y => f"$y%02d")
        country <- Gen.pick(2, 'A' to 'Z')
        serial  <- Gen.pick(13, ('A' to 'Z') ++ ('0' to '9'))
      } yield MovementReferenceNumber(year, country.mkString, serial.mkString)
    }

  implicit lazy val arbitraryProcedureType: Arbitrary[ProcedureType] =
    Arbitrary {
      Gen.oneOf(ProcedureType.Normal, ProcedureType.Simplified)
    }

  implicit lazy val arbitraryTraderWithEori: Arbitrary[TraderWithEori] =
    Arbitrary {

      for {
        eori            <- stringsWithMaxLength(17)
        name            <- Gen.option(stringsWithMaxLength(35))
        streetAndNumber <- Gen.option(stringsWithMaxLength(35))
        postCode        <- Gen.option(stringsWithMaxLength(9))
        city            <- Gen.option(stringsWithMaxLength(35))
        countryCode     <- Gen.option(stringsWithMaxLength(2))
      } yield TraderWithEori(eori, name, streetAndNumber, postCode, city, countryCode)
    }

  implicit lazy val arbitraryTraderWithoutEori: Arbitrary[TraderWithoutEori] =
    Arbitrary {

      for {
        name            <- stringsWithMaxLength(35)
        streetAndNumber <- stringsWithMaxLength(35)
        postCode        <- stringsWithMaxLength(9)
        city            <- stringsWithMaxLength(35)
        countryCode     <- stringsWithMaxLength(2)
      } yield TraderWithoutEori(name, streetAndNumber, postCode, city, countryCode)
    }

  implicit lazy val arbitraryTrader: Arbitrary[Trader] =
    Arbitrary {
      Gen.oneOf(arbitrary[TraderWithEori], arbitrary[TraderWithoutEori])
    }

  implicit lazy val arbitraryEndorsement: Arbitrary[Endorsement] =
    Arbitrary {

      for {
        date      <- Gen.option(datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now))
        authority <- Gen.option(stringsWithMaxLength(35))
        place     <- Gen.option(stringsWithMaxLength(35))
        country   <- Gen.option(stringsWithMaxLength(2))
      } yield Endorsement(date, authority, place, country)
    }

  implicit lazy val arbitraryIncident: Arbitrary[Incident] =
    Arbitrary {

      for {
        information <- Gen.option(stringsWithMaxLength(350))
        endorsement <- arbitrary[Endorsement]
      } yield Incident(information, endorsement)
    }

  implicit lazy val arbitraryContainer: Arbitrary[Container] = Arbitrary {
    for {
      containerNumber <- stringsWithMaxLength(17)
    } yield Container(containerNumber)
  }

  implicit lazy val arbitraryVehicularTranshipment: Arbitrary[VehicularTranshipment] =
    Arbitrary {

      for {
        transportIdentity  <- stringsWithMaxLength(27)
        transportCountry   <- stringsWithMaxLength(2)
        endorsement        <- arbitrary[Endorsement]
        numberOfContainers <- Gen.choose[Int](1, 99)
        containers         <- Gen.option(Gen.listOfN(numberOfContainers, arbitrary[Container]))
      } yield VehicularTranshipment(transportIdentity, transportCountry, endorsement, containers)
    }

  implicit lazy val arbitraryContainerTranshipment: Arbitrary[ContainerTranshipment] =
    Arbitrary {

      for {
        endorsement        <- arbitrary[Endorsement]
        numberOfContainers <- Gen.choose[Int](1, 99)
        containers         <- Gen.listOfN(numberOfContainers, arbitrary[Container])
      } yield ContainerTranshipment(endorsement, containers)
    }

  implicit lazy val arbitraryTranshipment: Arbitrary[Transhipment] =
    Arbitrary {
      Gen.oneOf[Transhipment](
        arbitrary[VehicularTranshipment],
        arbitrary[ContainerTranshipment]
      )
    }

  implicit lazy val arbitraryEventDetails: Arbitrary[EventDetails] =
    Arbitrary {
      Gen.oneOf[EventDetails](
        arbitrary[Incident],
        arbitrary[Transhipment]
      )
    }

  implicit lazy val arbitraryEnRouteEvent: Arbitrary[EnRouteEvent] =
    Arbitrary {

      for {
        place         <- stringsWithMaxLength(35)
        countryCode   <- stringsWithMaxLength(2)
        alreadyInNcts <- arbitrary[Boolean]
        eventDetails  <- arbitrary[EventDetails]
        numberOfSeals <- Gen.choose[Int](0, 99)
        seals         <- Gen.option(Gen.listOfN(numberOfSeals, stringsWithMaxLength(20)))
      } yield {

        val removeEmptySealsList = seals match {
          case Some(seals) if seals.nonEmpty => Some(seals)
          case _                             => None
        }

        EnRouteEvent(place, countryCode, alreadyInNcts, eventDetails, removeEmptySealsList)
      }
    }

  implicit lazy val arbitraryRejectionError: Arbitrary[RejectionError] =
    Arbitrary {

      for {
        errorType     <- arbitrary[Int]
        pointer       <- arbitrary[String]
        reason        <- arbitrary[Option[String]]
        originalValue <- arbitrary[Option[String]]
      } yield RejectionError(errorType, pointer, reason, originalValue)
    }
}
