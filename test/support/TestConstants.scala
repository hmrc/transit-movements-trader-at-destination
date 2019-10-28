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

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import models.{Trader, TraderWithEori, TraderWithoutEori}
import models.messages.NormalNotification
import models.messages.xml._
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

  def meta(messageSender: String, dateFormatted: String, timeFormatted: String, interchangeControlReference: String) = Meta(
    mesSenMES3 = messageSender,
    senIdeCodQuaMES4 = None,
    recIdeCodQuaMES7 = None,
    datOfPreMES9 = dateFormatted,
    timOfPreMES10 = timeFormatted,
    intConRefMES11 = interchangeControlReference,
    recRefMES12 = None,
    recRefQuaMES13 = None,
    appRefMES14 = Some("NCTS"),
    priMES15 = None,
    ackReqMES16 = None,
    comAgrIdMES17 = None,
    tesIndMES18 = Some("0"),
    mesIdeMES19 = "1",
    mesTypMES20 = "GB007A",
    comAccRefMES21 = None,
    mesSeqNumMES22 = None,
    firAndLasTraMES23 = None)

  def header(notification: NormalNotification) = Header(
    docNumHEA5 = notification.movementReferenceNumber,
    cusSubPlaHEA66 = notification.customsSubPlace,
    arrNotPlaHEA60 = notification.notificationPlace,
    arrNotPlaHEA60LNG = Some("EN"),
    arrAgrLocCodHEA62 = None,
    arrAgrLocOfGooHEA63 = None,
    arrAgrLocOfGooHEA63LNG = Some("EN"),
    arrAutLocOfGooHEA65 = None,
    simProFlaHEA132 = None,
    arrNotDatHEA141 = Format.dateFormatted(notification.notificationDate),
    diaLanIndAtDesHEA255 = None)

  def traderDestination(trader:Trader) = trader match {
    case traderWithEori: TraderWithEori =>
      TraderDestination(
        namTRD7String = traderWithEori.name,
        strAndNumTRD22 = traderWithEori.streetAndNumber,
        posCodTRD23 = traderWithEori.postCode,
        citTRD24 = traderWithEori.city,
        couTRD25 = traderWithEori.countryCode,
        tintrd59 = Some(traderWithEori.eori))
    case traderWithoutEori: TraderWithoutEori =>
      TraderDestination(
        namTRD7String = Some(traderWithoutEori.name),
        strAndNumTRD22 = Some(traderWithoutEori.streetAndNumber),
        posCodTRD23 = Some(traderWithoutEori.postCode),
        citTRD24 = Some(traderWithoutEori.city),
        couTRD25 = Some(traderWithoutEori.countryCode),
        tintrd59 = None)
  }

  def customsOffice(notification: NormalNotification) = CustomsOfficeOfPresentation(
    refNumRES1 = notification.presentationOffice
  )
}
