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

import config.AppConfig
import models.messages.xml._
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import utils.Format

class ConvertToSubmissionModelSpec extends FreeSpec with MustMatchers with GuiceOneAppPerSuite  {

  import support.TestConstants._

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToXml = new ConvertToSubmissionModel(appConfig)

  "ConvertToSubmissionModel" - {

    "convert NormalNotification to ArrivalNotificationXml" in {

      val notification = normalArrivalNotification(traderWithoutEori)

      val localDateTime: LocalDateTime = LocalDateTime.now()

      val timeFormatted = Format.timeFormatted(localDateTime)
      val dateFormatted = Format.dateFormatted(localDateTime)

      //TODO double check this value
      val interchangeControlReference = s"WE+${dateFormatted}-1"

      val meta = Meta(
        rootNode = "CC007A",
        nameSpace = Map(
          "xmlns=" -> "http://ncts.dgtaxud.ec/CC007A",
          "xmlns:xsi=" -> "http://www.w3.org/2001/XMLSchema-instance",
          "xmlns:complex_ncts=" -> "http://ncts.dgtaxud.ec/complex_ncts",
          "xsi:schemaLocation=" -> "http://ncts.dgtaxud.ec/CC007A"),
        synIdeMES1 = "UNOC",
        synVerNumMES2 = "3",
        mesSenMES3 = appConfig.env,
        senIdeCodQuaMES4 = None,
        mesRecMES6 = "NCTS",
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

      val header = Header(
        docNumHEA5 = notification.movementReferenceNumber,
        cusSubPlaHEA66 = None,
        arrNotPlaHEA60 = notification.notificationPlace,
        arrNotPlaHEA60LNG = None,
        arrAgrLocCodHEA62 = None,
        arrAgrLocOfGooHEA63 = None,
        arrAgrLocOfGooHEA63LNG = None,
        arrAutLocOfGooHEA65 = None,
        simProFlaHEA132 = None,
        arrNotDatHEA141 = Format.dateFormatted(notification.notificationDate),
        diaLanIndAtDesHEA255 = None)

      val traderDestination = TraderDestination(
        namTRD7String = Some(traderWithoutEori.name),
        strAndNumTRD22 = Some(traderWithoutEori.streetAndNumber),
        posCodTRD23 = Some(traderWithoutEori.postCode),
        citTRD24 = Some(traderWithoutEori.city),
        couTRD25 = Some(traderWithoutEori.countryCode),
        nadlngrd = None,
        tintrd59 = None)

      val customsOffice = CustomsOfficeOfPresentation(
        refNumRES1 = notification.presentationOffice
      )

      convertToXml.convert(notification, localDateTime) mustBe
        ArrivalNotificationXml(meta, header, traderDestination, customsOffice)
    }

  }

}