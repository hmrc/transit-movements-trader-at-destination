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

package support

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import models.messages.NormalNotification
import models.messages.request._
import models.{Trader, TraderWithEori, TraderWithoutEori}
import utils.Format

object TestConstants {

  val date: LocalDate = LocalDate.now
  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
  val dateFormatted: String = date.format(dateTimeFormatter)

  lazy val traderWithEori = TraderWithEori("GB163910077000", None, None, None, None, None)
  lazy val traderWithoutEori = TraderWithoutEori("name", "street", "postcode", "city", "country code")

  def normalArrivalNotification(trader: Trader) = NormalNotification(
    movementReferenceNumber = "19IT02110010007827",
    notificationPlace = "DOVER",
    notificationDate = date,
    customsSubPlace =  None,
    trader = trader,
    presentationOffice = "GB000060",
    enRouteEvents = Seq.empty)

  def meta(
            eori: String,
            environment: String,
            prefix: String,
            dateTime: LocalDateTime
          ): Meta = {

    Meta(
      messageSender = MessageSender(environment, eori),
//      dateOfPreparation = Format.dateFormatted(dateTime),
//      timeOfPreparation = Format.timeFormatted(dateTime),
      interchangeControlReference = InterchangeControlReference(prefix, dateTime)
    )
  }

  def header(notification: NormalNotification) = Header(
    movementReferenceNumber = notification.movementReferenceNumber,
    customsSubPlace = notification.customsSubPlace,
    arrivalNotificationPlace = notification.notificationPlace,
    arrivalAgreedLocationOfGoods = None,
    simplifiedProcedureFlag = "0"
  )

  def traderDestination(trader:Trader): TraderDestination = trader match {
    case traderWithEori: TraderWithEori =>
      TraderDestination(
        name = traderWithEori.name,
        streetAndNumber = traderWithEori.streetAndNumber,
        postCode = traderWithEori.postCode,
        city = traderWithEori.city,
        countryCode = traderWithEori.countryCode,
        eori = Some(traderWithEori.eori))
    case traderWithoutEori: TraderWithoutEori =>
      TraderDestination(
        name = Some(traderWithoutEori.name),
        streetAndNumber = Some(traderWithoutEori.streetAndNumber),
        postCode = Some(traderWithoutEori.postCode),
        city = Some(traderWithoutEori.city),
        countryCode = Some(traderWithoutEori.countryCode),
        eori = None)
  }

  def customsOffice(notification: NormalNotification) = CustomsOfficeOfPresentation(
    presentationOffice = notification.presentationOffice
  )
}
