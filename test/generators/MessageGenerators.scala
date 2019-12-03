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

import java.time.LocalDate

import models.messages._
import models.messages.request._
import models.EnRouteEvent
import models.MovementReferenceNumber
import models.RejectionError
import models.Trader
import models.TraderWithEori
import models.TraderWithoutEori
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait MessageGenerators extends ModelGenerators {

  implicit lazy val validXmlString: Arbitrary[String] =
    Arbitrary(arbitrary[List[Char]] map {
      _.mkString
        .replaceAll("([&<>]|\\p{C})", "")
    })

  implicit lazy val arbitraryCustomsOfficeOfPresentation: Arbitrary[CustomsOfficeOfPresentation] = {
    Arbitrary {

      for {
        presentationOffice <- stringsWithMaxLength(8)
      } yield CustomsOfficeOfPresentation(presentationOffice)
    }
  }

  implicit lazy val arbitraryGoodsReleaseNotification: Arbitrary[GoodsReleaseNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[MovementReferenceNumber].map(_.toString())
        releaseDate        <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        trader             <- arbitrary[Trader]
        presentationOffice <- stringsWithMaxLength(8)
      } yield GoodsReleaseNotification(mrn, releaseDate, trader, presentationOffice)
    }

  implicit lazy val arbitraryArrivalNotificationRejection: Arbitrary[ArrivalNotificationRejection] =
    Arbitrary {

      for {
        mrn    <- arbitrary[MovementReferenceNumber].map(_.toString())
        date   <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        action <- arbitrary[Option[String]]
        reason <- arbitrary[Option[String]]
        errors <- arbitrary[Seq[RejectionError]]
      } yield ArrivalNotificationRejection(mrn, date, action, reason, errors)
    }

  implicit lazy val arbitraryNormalNotification: Arbitrary[NormalNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[MovementReferenceNumber].map(_.toString())
        place              <- stringsWithMaxLength(35)
        date               <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        subPlace           <- Gen.option(stringsWithMaxLength(17))
        trader             <- arbitrary[Trader]
        presentationOffice <- stringsWithMaxLength(8)
        events             <- seqWithMaxLength[EnRouteEvent](9)
      } yield NormalNotification(mrn, place, date, subPlace, trader, presentationOffice, Option(events))
    }

  implicit lazy val arbitrarySimplifiedNotification: Arbitrary[SimplifiedNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[MovementReferenceNumber].map(_.toString())
        place              <- stringsWithMaxLength(35)
        date               <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        approvedLocation   <- Gen.option(stringsWithMaxLength(17))
        trader             <- arbitrary[Trader]
        presentationOffice <- stringsWithMaxLength(8)
        events             <- seqWithMaxLength[EnRouteEvent](9)
      } yield SimplifiedNotification(mrn, place, date, approvedLocation, trader, presentationOffice, Option(events))
    }

  implicit lazy val arbitraryArrivalNotification: Arbitrary[ArrivalNotification] =
    Arbitrary {

      Gen.oneOf(arbitrary[NormalNotification], arbitrary[SimplifiedNotification])
    }

  implicit lazy val arbitraryTraderDestination: Arbitrary[TraderDestination] = {
    Arbitrary {

      for {
        name            <- Gen.option(stringsWithMaxLength(35))
        streetAndNumber <- Gen.option(stringsWithMaxLength(35))
        postCode        <- Gen.option(stringsWithMaxLength(9))
        city            <- Gen.option(stringsWithMaxLength(35))
        countryCode     <- Gen.option(stringsWithMaxLength(2))
        eori            <- Gen.option(stringsWithMaxLength(17))
      } yield TraderDestination(name, streetAndNumber, postCode, city, countryCode, eori)

    }
  }

  implicit lazy val arbitraryMessageSender: Arbitrary[MessageSender] = {
    Arbitrary {
      for {
        environment <- arbitrary[String]
        eori        <- arbitrary[String]
      } yield MessageSender(environment, eori)
    }
  }

  implicit lazy val arbitraryInterchangeControlReference: Arbitrary[InterchangeControlReference] = {
    Arbitrary {
      for {
        dateTime <- arbitrary[String]
        index    <- arbitrary[Int]
      } yield InterchangeControlReference(dateTime, index)
    }
  }

  implicit lazy val arbitraryMeta: Arbitrary[Meta] = {
    Arbitrary {
      for {
        messageSender               <- arbitrary[MessageSender]
        interchangeControlReference <- arbitrary[InterchangeControlReference]
      } yield
        Meta(
          messageSender,
          interchangeControlReference,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
    }
  }

  implicit lazy val arbitraryHeader: Arbitrary[Header] = {
    Arbitrary {
      for {
        movementReferenceNumber  <- arbitrary[MovementReferenceNumber].map(_.toString())
        customsSubPlace          <- Gen.option(stringsWithMaxLength(17))
        arrivalNotificationPlace <- stringsWithMaxLength(35)
        simplifiedProcedureFlag  <- Gen.oneOf("0", "1")
      } yield Header(movementReferenceNumber, customsSubPlace, arrivalNotificationPlace, None, simplifiedProcedureFlag)
    }
  }

  implicit lazy val arbitraryArrivalNotificationRequest: Arbitrary[ArrivalNotificationRequest] = {
    Arbitrary {
      for {
        meta              <- arbitrary[Meta]
        header            <- arbitrary[Header]
        traderDestination <- arbitrary[TraderDestination]
        customsOffice     <- arbitrary[CustomsOfficeOfPresentation]
        enRouteEvents     <- Gen.option(arbitrary[Seq[EnRouteEvent]])
      } yield ArrivalNotificationRequest(meta, header, traderDestination, customsOffice, enRouteEvents)
    }
  }

  val arbitraryArrivalNotificationRequestWithEori: Gen[ArrivalNotificationRequest] = {
    for {
      meta           <- arbitrary[Meta]
      header         <- arbitrary[Header]
      traderWithEori <- arbitrary[TraderWithEori]
      customsOffice  <- arbitrary[CustomsOfficeOfPresentation]
      headerWithProcedure = header.copy(simplifiedProcedureFlag = "0")
      traderDestination = {
        TraderDestination(
          traderWithEori.name,
          traderWithEori.streetAndNumber,
          traderWithEori.postCode,
          traderWithEori.city,
          traderWithEori.countryCode,
          Some(traderWithEori.eori)
        )
      }
      enRouteEvents <- Gen.option(arbitrary[Seq[EnRouteEvent]])

    } yield ArrivalNotificationRequest(meta, headerWithProcedure, traderDestination, customsOffice, enRouteEvents)
  }

  val arbitraryArrivalNotificationRequestWithoutEori: Gen[ArrivalNotificationRequest] = {
    for {
      meta              <- arbitrary[Meta]
      header            <- arbitrary[Header]
      traderWithoutEori <- arbitrary[TraderWithoutEori]
      customsOffice     <- arbitrary[CustomsOfficeOfPresentation]
      headerWithProcedure = header.copy(simplifiedProcedureFlag = "0")
      traderDestination = {
        TraderDestination(
          Some(traderWithoutEori.name),
          Some(traderWithoutEori.streetAndNumber),
          Some(traderWithoutEori.postCode),
          Some(traderWithoutEori.city),
          Some(traderWithoutEori.countryCode),
          None
        )
      }
      enRouteEvents <- Gen.option(arbitrary[Seq[EnRouteEvent]])

    } yield ArrivalNotificationRequest(meta, headerWithProcedure, traderDestination, customsOffice, enRouteEvents)
  }

}
