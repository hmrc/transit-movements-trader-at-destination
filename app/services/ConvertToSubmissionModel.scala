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

package services

import java.time.LocalDateTime

import com.google.inject.Inject
import config.AppConfig
import models.messages.NormalNotification
import models.messages.xml._
import models.{Trader, TraderWithEori, TraderWithoutEori}
import utils.Format._

class ConvertToSubmissionModel @Inject()(appConfig: AppConfig){

  /**
    * Conversion layer accepts internal case class
    * converts to xml model
    * returns string of xml
    */

  def convert[A](x: A, time: LocalDateTime): ArrivalNotificationXml = x match {
    case arrivalNotification: NormalNotification => {

      val meta = buildMeta(time)
      val header = buildHeader(arrivalNotification)
      val traderDestination = buildTrader(arrivalNotification.trader)
      val customsOffice = CustomsOfficeOfPresentation(
        refNumRES1 = arrivalNotification.presentationOffice
      )

      ArrivalNotificationXml(meta, header, traderDestination, customsOffice)
    }
    case _ => {
      throw new RuntimeException
    }

  }

  private def buildMeta(time: LocalDateTime): Meta = {

    //TODO double check this value
    val interchangeControlReference = s"WE+${dateFormatted(time)}-1"

    Meta(
      rootNode = ArrivalNotificationXml.rootNode,
      nameSpace = ArrivalNotificationXml.nameSpace,
      synIdeMES1 = ArrivalNotificationXml.syntaxIdentifier,
      synVerNumMES2 = ArrivalNotificationXml.syntaxVersionNumber,
      mesSenMES3 = appConfig.env , //"environment specific id + Trader TURN", //TODO: Get environment id and tag on eori
      senIdeCodQuaMES4 = None,
      mesRecMES6 = ArrivalNotificationXml.messageRecipient,
      recIdeCodQuaMES7 = None,
      datOfPreMES9 = dateFormatted(time),
      timOfPreMES10 = timeFormatted(time),
      intConRefMES11 = interchangeControlReference,
      recRefMES12 = None,
      recRefQuaMES13 = None,
      appRefMES14 = Some(ArrivalNotificationXml.applicationReference),
      priMES15 = None,
      ackReqMES16 = None,
      comAgrIdMES17 = None,
      tesIndMES18 = Some(ArrivalNotificationXml.testIndicator),
      mesIdeMES19 = ArrivalNotificationXml.messageIdentification,
      mesTypMES20 = ArrivalNotificationXml.messageCode,
      comAccRefMES21 = None,
      mesSeqNumMES22 = None,
      firAndLasTraMES23 = None)
  }

  private def buildHeader(arrivalNotification: NormalNotification): Header = {
    Header(
      docNumHEA5 = arrivalNotification.movementReferenceNumber,
      cusSubPlaHEA66 = arrivalNotification.customsSubPlace,
      arrNotPlaHEA60 = arrivalNotification.notificationPlace,
      arrNotPlaHEA60LNG = None,
      arrAgrLocCodHEA62 = None,
      arrAgrLocOfGooHEA63 = None,
      arrAgrLocOfGooHEA63LNG = None,
      arrAutLocOfGooHEA65 = None,
      simProFlaHEA132 = None,
      arrNotDatHEA141 = dateFormatted(arrivalNotification.notificationDate),
      diaLanIndAtDesHEA255 = None
    )
  }

  private def buildTrader(trader: Trader): TraderDestination = trader match {
    case traderWithEori: TraderWithEori =>
      TraderDestination(
        namTRD7String = traderWithEori.name,
        strAndNumTRD22 = traderWithEori.streetAndNumber,
        posCodTRD23 = traderWithEori.postCode,
        citTRD24 = traderWithEori.city,
        couTRD25 = traderWithEori.countryCode,
        nadlngrd = None,
        tintrd59 = None)
    case traderWithoutEori: TraderWithoutEori =>
      TraderDestination(
        namTRD7String = Some(traderWithoutEori.name),
        strAndNumTRD22 = Some(traderWithoutEori.streetAndNumber),
        posCodTRD23 = Some(traderWithoutEori.postCode),
        citTRD24 = Some(traderWithoutEori.city),
        couTRD25 = Some(traderWithoutEori.countryCode),
        nadlngrd = None,
        tintrd59 = None)
  }

}