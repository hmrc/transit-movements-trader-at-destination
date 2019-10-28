/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}

import models._
import models.messages._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

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

  implicit lazy val arbitraryProcedureType: Arbitrary[ProcedureType] =
    Arbitrary {
      Gen.oneOf(ProcedureType.Normal, ProcedureType.Simplified)
    }

  implicit lazy val arbitraryTraderWithEori: Arbitrary[TraderWithEori] =
    Arbitrary {

      for {
        eori            <- arbitrary[String]
        name            <- Gen.option(arbitrary[String])
        streetAndNumber <- Gen.option(arbitrary[String])
        postCode        <- Gen.option(arbitrary[String])
        city            <- Gen.option(arbitrary[String])
        countryCode     <- Gen.option(arbitrary[String])
      } yield TraderWithEori(eori, name, streetAndNumber, postCode, city, countryCode)
    }

  implicit lazy val arbitraryTraderWithoutEori: Arbitrary[TraderWithoutEori] =
    Arbitrary {

      for {
        name            <- arbitrary[String]
        streetAndNumber <- arbitrary[String]
        postCode        <- arbitrary[String]
        city            <- arbitrary[String]
        countryCode     <- arbitrary[String]
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
        authority <- Gen.option(arbitrary[String])
        place     <- Gen.option(arbitrary[String])
        country   <- Gen.option(arbitrary[String])
      } yield Endorsement(date, authority, place, country)
    }

  implicit lazy val arbitraryIncident: Arbitrary[Incident] =
    Arbitrary {

      for {
        information <- arbitrary[Option[String]]
        endorsement <- arbitrary[Endorsement]
      } yield Incident(information, endorsement)
    }

  implicit lazy val arbitraryVehicularTranshipment: Arbitrary[VehicularTranshipment] =
    Arbitrary {

      for {
        transportIdentity <- arbitrary[String]
        transportCountry  <- arbitrary[String]
        endorsement       <- arbitrary[Endorsement]
        containers        <- arbitrary[Seq[String]]
      } yield VehicularTranshipment(transportIdentity, transportCountry, endorsement, containers)
    }

  implicit lazy val arbitraryContainerTranshipment: Arbitrary[ContainerTranshipment] =
    Arbitrary {

      for {
        endorsement       <- arbitrary[Endorsement]
        containers        <- arbitrary[Seq[String]].suchThat(_.nonEmpty)
      } yield ContainerTranshipment(endorsement, containers)
    }

  implicit lazy val arbitraryTranshipment: Arbitrary[Transhipment] =
    Arbitrary{
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
        place         <- arbitrary[String]
        countryCode   <- arbitrary[String]
        alreadyInNcts <- arbitrary[Boolean]
        eventDetails  <- arbitrary[EventDetails]
        seals         <- arbitrary[Seq[String]]
      } yield EnRouteEvent(place, countryCode, alreadyInNcts, eventDetails, seals)
    }

  implicit lazy val arbitraryNormalNotification: Arbitrary[NormalNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[String]
        place              <- arbitrary[String]
        date               <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        subPlace           <- arbitrary[Option[String]]
        trader             <- arbitrary[Trader]
        presentationOffice <- arbitrary[String]
        events             <- arbitrary[Seq[EnRouteEvent]]
      } yield NormalNotification(mrn, place, date, subPlace, trader, presentationOffice, events)
    }

  implicit lazy val arbitrarySimplifiedNotification: Arbitrary[SimplifiedNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[String]
        place              <- arbitrary[String]
        date               <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        approvedLocation   <- arbitrary[Option[String]]
        trader             <- arbitrary[Trader]
        presentationOffice <- arbitrary[String]
        events             <- arbitrary[Seq[EnRouteEvent]]
      } yield SimplifiedNotification(mrn, place, date, approvedLocation, trader, presentationOffice, events)
    }

  implicit lazy val arbitraryArrivalNotification: Arbitrary[ArrivalNotification] =
    Arbitrary {

      Gen.oneOf(arbitrary[NormalNotification], arbitrary[SimplifiedNotification])
    }

  implicit lazy  val arbitraryRejectionError: Arbitrary[RejectionError] =
    Arbitrary {

      for {
        errorType     <- arbitrary[Int]
        pointer       <- arbitrary[String]
        reason        <- arbitrary[Option[String]]
        originalValue <- arbitrary[Option[String]]
      } yield RejectionError(errorType, pointer, reason, originalValue)
    }

  implicit lazy val arbitraryArrivalNotificationRejection: Arbitrary[ArrivalNotificationRejection] =
    Arbitrary {

      for {
        mrn    <- arbitrary[String]
        date   <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        action <- arbitrary[Option[String]]
        reason <- arbitrary[Option[String]]
        errors <- arbitrary[Seq[RejectionError]]
      } yield ArrivalNotificationRejection(mrn, date, action, reason, errors)
    }

  implicit lazy val arbitraryGoodsReleaseNotification: Arbitrary[GoodsReleaseNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[String]
        releaseDate        <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        trader             <- arbitrary[Trader]
        presentationOffice <- arbitrary[String]
      } yield GoodsReleaseNotification(mrn, releaseDate, trader, presentationOffice)
    }

}
