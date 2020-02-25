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

import models.messages._
import models.request._
import models.ArrivalMovement
import models.Message
import models.RejectionError
import models.request
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait MessageGenerators extends ModelGenerators {

  private val pastDate: LocalDate = LocalDate.of(1900, 1, 1)
  private val dateNow: LocalDate  = LocalDate.now

  implicit lazy val arbitraryMessage: Arbitrary[Message] = {
    Arbitrary {
      for {
        date    <- datesBetween(pastDate, dateNow)
        time    <- timesBetween(pastDate, dateNow)
        message <- arbitrary[ArrivalNotificationMessage]

      } yield Message(date, time, message)
    }
  }

  implicit lazy val arbitraryArrivalMovement: Arbitrary[ArrivalMovement] = {
    Arbitrary {
      for {
        movementReferenceId     <- arbitrary[Int]
        movementReferenceNumber <- arbitrary[MovementReferenceNumber]
        messages                <- seqWithMaxLength[Message](20)
      } yield
        ArrivalMovement(
          movementReferenceId,
          movementReferenceNumber.toString,
          messages
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

  implicit lazy val arbitraryCustomsOfficeOfPresentation: Arbitrary[CustomsOfficeOfPresentation] = {
    Arbitrary {

      for {
        presentationOffice <- stringsWithMaxLength(CustomsOfficeOfPresentation.Constants.presentationOfficeLength)
      } yield CustomsOfficeOfPresentation(presentationOffice)
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

  implicit lazy val arbitraryArrivalNotificationRejection: Arbitrary[ArrivalNotificationRejectionMessage] =
    Arbitrary {

      for {
        mrn    <- arbitrary[MovementReferenceNumber].map(_.toString())
        date   <- datesBetween(pastDate, dateNow)
        action <- arbitrary[Option[String]]
        reason <- arbitrary[Option[String]]
        errors <- arbitrary[Seq[RejectionError]]
      } yield ArrivalNotificationRejectionMessage(mrn, date, action, reason, errors)
    }

  implicit lazy val arbitraryNormalNotification: Arbitrary[NormalNotificationMessage] =
    Arbitrary {

      for {
        mrn                    <- arbitrary[MovementReferenceNumber].map(_.toString())
        place                  <- stringsWithMaxLength(NormalNotificationMessage.Constants.notificationPlaceLength)
        date                   <- datesBetween(pastDate, dateNow)
        subPlace               <- Gen.option(stringsWithMaxLength(NormalNotificationMessage.Constants.customsSubPlaceLength))
        trader                 <- arbitrary[Trader]
        presentationOfficeId   <- stringsWithMaxLength(NormalNotificationMessage.Constants.presentationOfficeIdLength)
        presentationOfficeName <- arbitrary[String]
        events                 <- Gen.option(seqWithMaxLength[EnRouteEvent](NormalNotificationMessage.Constants.maxNumberOfEnRouteEvents))
      } yield models.messages.NormalNotificationMessage(mrn, place, date, subPlace, trader, presentationOfficeId, presentationOfficeName, events)
    }

  implicit lazy val arbitrarySimplifiedNotification: Arbitrary[SimplifiedNotificationMessage] =
    Arbitrary {

      for {
        mrn                    <- arbitrary[MovementReferenceNumber].map(_.toString())
        place                  <- stringsWithMaxLength(SimplifiedNotificationMessage.Constants.notificationPlaceLength)
        date                   <- datesBetween(pastDate, dateNow)
        approvedLocation       <- Gen.option(stringsWithMaxLength(SimplifiedNotificationMessage.Constants.approvedLocationLength))
        trader                 <- arbitrary[Trader]
        presentationOfficeId   <- stringsWithMaxLength(NormalNotificationMessage.Constants.presentationOfficeIdLength)
        presentationOfficeName <- arbitrary[String]
        events                 <- Gen.option(seqWithMaxLength[EnRouteEvent](SimplifiedNotificationMessage.Constants.maxNumberOfEnRouteEvents))
      } yield models.messages.SimplifiedNotificationMessage(mrn, place, date, approvedLocation, trader, presentationOfficeId, presentationOfficeName, events)
    }

  implicit lazy val arbitraryArrivalNotification: Arbitrary[ArrivalNotificationMessage] =
    Arbitrary {

      Gen.oneOf(arbitrary[NormalNotificationMessage], arbitrary[SimplifiedNotificationMessage])
    }

  implicit lazy val arbitraryTraderDestination: Arbitrary[TraderDestination] = {
    Arbitrary {

      for {
        name            <- Gen.option(stringsWithMaxLength(TraderDestination.Constants.nameLength))
        streetAndNumber <- Gen.option(stringsWithMaxLength(TraderDestination.Constants.streetAndNumberLength))
        postCode        <- Gen.option(stringsWithMaxLength(TraderDestination.Constants.postCodeLength))
        city            <- Gen.option(stringsWithMaxLength(TraderDestination.Constants.cityLength))
        countryCode     <- Gen.option(stringsWithMaxLength(TraderDestination.Constants.countryCodeLength))
        eori            <- Gen.option(stringsWithMaxLength(TraderDestination.Constants.eoriLength))
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
        request.Meta(
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

  implicit lazy val arbitraryProcedureTypeFlag: Arbitrary[ProcedureTypeFlag] = {
    Arbitrary {
      for {
        procedureType <- Gen.oneOf(Seq(SimplifiedProcedureFlag, NormalProcedureFlag))
      } yield procedureType
    }
  }

  implicit lazy val arbitraryHeader: Arbitrary[Header] = {
    Arbitrary {
      for {
        movementReferenceNumber  <- arbitrary[MovementReferenceNumber].map(_.toString())
        customsSubPlace          <- Gen.option(stringsWithMaxLength(Header.Constants.customsSubPlaceLength))
        arrivalNotificationPlace <- stringsWithMaxLength(Header.Constants.arrivalNotificationPlaceLength)
        procedureTypeFlag        <- arbitrary[ProcedureTypeFlag]
      } yield Header(movementReferenceNumber, customsSubPlace, arrivalNotificationPlace, None, procedureTypeFlag)
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
      } yield request.ArrivalNotificationRequest(meta, header, traderDestination, customsOffice, enRouteEvents)
    }
  }

  val arbitraryArrivalNotificationRequestWithEori: Gen[ArrivalNotificationRequest] = {
    for {
      meta           <- arbitrary[Meta]
      header         <- arbitrary[Header]
      traderWithEori <- arbitrary[TraderWithEori]
      customsOffice  <- arbitrary[CustomsOfficeOfPresentation]
      headerWithProcedure = header.copy(procedureTypeFlag = NormalProcedureFlag)
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

    } yield request.ArrivalNotificationRequest(meta, headerWithProcedure, traderDestination, customsOffice, enRouteEvents)
  }

  val arbitraryArrivalNotificationRequestWithoutEori: Gen[ArrivalNotificationRequest] = {
    for {
      meta              <- arbitrary[Meta]
      header            <- arbitrary[Header]
      traderWithoutEori <- arbitrary[TraderWithoutEori]
      customsOffice     <- arbitrary[CustomsOfficeOfPresentation]
      headerWithProcedure = header.copy(procedureTypeFlag = NormalProcedureFlag)
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

    } yield request.ArrivalNotificationRequest(meta, headerWithProcedure, traderDestination, customsOffice, enRouteEvents)
  }

}
