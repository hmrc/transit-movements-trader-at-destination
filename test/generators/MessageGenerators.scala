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

import java.time.{LocalDate, LocalDateTime}

import models.messages._
import models.messages.request._
import models.{EnRouteEvent, RejectionError, Trader, TraderWithEori, TraderWithoutEori}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}


trait MessageGenerators extends ModelGenerators {


  implicit lazy val validXmlString: Arbitrary[String] =
    Arbitrary(arbitrary[List[Char]] map {
      _.mkString
        .replaceAll("([&<>]|\\p{C})", "")
    })


  implicit lazy val arbitraryCustomsOfficeOfPresentation: Arbitrary[CustomsOfficeOfPresentation] = {
    Arbitrary {

      for {
        presentationOffice  <- arbitrary[String]
      } yield CustomsOfficeOfPresentation(presentationOffice)
    }
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

  implicit lazy val arbitraryNormalNotification: Arbitrary[NormalNotification] =
    Arbitrary {

      for {
        mrn                <- arbitrary[String]
        place              <- arbitrary[String]
        date               <- datesBetween(LocalDate.of(1900, 1, 1), LocalDate.now)
        subPlace           <- arbitrary[Option[String]]
        trader             <- arbitrary[Trader]
        presentationOffice <- arbitrary[String]
        events             <- Gen.listOf(arbitrary[EnRouteEvent])
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
        events             <- Gen.listOf(arbitrary[EnRouteEvent])
      } yield SimplifiedNotification(mrn, place, date, approvedLocation, trader, presentationOffice, events)
    }

  implicit lazy val arbitraryArrivalNotification: Arbitrary[ArrivalNotification] =
    Arbitrary {

      Gen.oneOf(arbitrary[NormalNotification], arbitrary[SimplifiedNotification])
    }

  implicit lazy val arbitraryTraderDestination: Arbitrary[TraderDestination] = {
    Arbitrary {

      for {
        name              <- Gen.option(arbitrary[String])
        streetAndNumber   <- Gen.option(arbitrary[String])
        postCode          <- Gen.option(arbitrary[String])
        city              <- Gen.option(arbitrary[String])
        countryCode       <- Gen.option(arbitrary[String])
        eori              <- Gen.option(arbitrary[String])
      } yield TraderDestination(name, streetAndNumber, postCode, city, countryCode, eori)

    }
  }

  implicit lazy val arbitraryMessageSender: Arbitrary[MessageSender] = {
    Arbitrary {
      for {
        environment <- arbitrary[String]
        eori <- arbitrary[String]
      } yield MessageSender(environment, eori)
    }
  }

  implicit lazy val arbitraryInterchangeControlReference: Arbitrary[InterchangeControlReference] = {
    Arbitrary {
      for {
        dateTime <- arbitrary[String]
        index <- arbitrary[Int]
      } yield InterchangeControlReference(dateTime, index)
    }
  }

  implicit lazy val arbitraryMeta: Arbitrary[Meta] = {
    Arbitrary {
      for {
        messageSender <- arbitrary[MessageSender]
        interchangeControlReference <- arbitrary[InterchangeControlReference]
      } yield Meta(
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
        movementReferenceNumber <- arbitrary[String]
        customsSubPlace <-  Gen.option(arbitrary[String])
        arrivalNotificationPlace <- arbitrary[String]
        simplifiedProcedureFlag <- Gen.oneOf("0", "1")
      } yield Header(
        movementReferenceNumber,
        customsSubPlace,
        arrivalNotificationPlace,
        None,
        simplifiedProcedureFlag)
    }
  }

  implicit lazy val arbitraryArrivalNotificationRequest: Arbitrary[ArrivalNotificationRequest] = {
    Arbitrary {
      for {
        meta <- arbitrary[Meta]
        header <- arbitrary[Header]
        traderDestination <- arbitrary[TraderDestination]
        customsOffice <- arbitrary[CustomsOfficeOfPresentation]
      } yield ArrivalNotificationRequest(meta, header, traderDestination, customsOffice)

    }
  }

  val arbitraryArrivalNotificationRequestWithEori: Gen[ArrivalNotificationRequest] = {
    for {
      meta <- arbitrary[Meta]
      header <- arbitrary[Header]
      traderWithEori <- arbitrary[TraderWithEori]
      customsOffice <- arbitrary[CustomsOfficeOfPresentation]
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
    } yield ArrivalNotificationRequest(meta, headerWithProcedure, traderDestination, customsOffice)
  }

  val arbitraryArrivalNotificationRequestWithoutEori: Gen[ArrivalNotificationRequest] = {
    for {
      meta <- arbitrary[Meta]
      header <- arbitrary[Header]
      traderWithoutEori <- arbitrary[TraderWithoutEori]
      customsOffice <- arbitrary[CustomsOfficeOfPresentation]
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
    } yield ArrivalNotificationRequest(meta, headerWithProcedure, traderDestination, customsOffice)
  }

}
