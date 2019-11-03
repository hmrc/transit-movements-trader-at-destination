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
import models.messages.request.ArrivalNotificationRequest
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import support.TestConstants._
import utils.Format

import scala.xml.Utility.trim
import scala.xml.{Node, NodeSeq}

class XmlBuilderServiceSpec extends FreeSpec with MustMatchers with GuiceOneAppPerSuite {

  val authenticatedUserEori = "NCTS_EU_EXIT"

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToXml = new XmlBuilderService()

  "XmlBuilderService" - {

    "must return correct nodes" in {

      val localDateTime: LocalDateTime = LocalDateTime.now

      val notification = normalArrivalNotification(traderWithEori)
      val metaData = meta(authenticatedUserEori, appConfig.env, "WE", localDateTime)
      val headerData = header(notification)
      val traderDestinationData = traderDestination(notification.trader)
      val customsOfficeData = customsOffice(notification)

      val dateOfPreperation = Format.dateFormatted(localDateTime)
      val timeOfPreperation = Format.timeFormatted(localDateTime)

      val arrivalNotificationRequest = ArrivalNotificationRequest(metaData, headerData, traderDestinationData, customsOfficeData)

      val validXml: Node = {
        trim(
          <CC007A
          xsi:schemaLocation="http://ncts.dgtaxud.ec/CC007A"
          xmlns="http://ncts.dgtaxud.ec/CC007A"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xmlns:complex_ncts="http://ncts.dgtaxud.ec/complex_ncts">
              <SynIdeMES1>UNOC</SynIdeMES1>
              <SynVerNumMES2>3</SynVerNumMES2>
              <MesSenMES3>{metaData.messageSender.toString}</MesSenMES3>
              <MesRecMES6>NCTS</MesRecMES6>
              <DatOfPreMES9>{dateOfPreperation}</DatOfPreMES9>
              <TimOfPreMES10>{timeOfPreperation}</TimOfPreMES10>
              <IntConRefMES11>{metaData.interchangeControlReference.toString}</IntConRefMES11>
              <AppRefMES14>NCTS</AppRefMES14>
              <MesIdeMES18>0</MesIdeMES18>
              <MesIdeMES19>1</MesIdeMES19>
              <MesTypMES20>GB007A</MesTypMES20>
              <HEAHEA>
                <DocNumHEA5>{notification.movementReferenceNumber}</DocNumHEA5>
                <ArrNotPlaHEA60>{notification.notificationPlace}</ArrNotPlaHEA60>
                <ArrNotPlaHEA60LNG>EN</ArrNotPlaHEA60LNG>
                <ArrAgrLocOfGooHEA63LNG>EN</ArrAgrLocOfGooHEA63LNG>
                <ArrivalAgreedLocationOfGoodsLNG>EN</ArrivalAgreedLocationOfGoodsLNG>
                <SimProFlaHEA132>{headerData.simplifiedProcedureFlag}</SimProFlaHEA132>
                <ArrNotDatHEA141>{dateOfPreperation}</ArrNotDatHEA141>
              </HEAHEA>
              <TRADESTRD>
                <NADLNGRD>EN</NADLNGRD>
                {
                  traderDestinationData.eori.map {
                    eori =>
                      <TINTRD59>{eori}</TINTRD59>
                  }.getOrElse(NodeSeq.Empty)
                }
              </TRADESTRD>
              <CUSOFFPREOFFRES>
                <RefNumRES1>{customsOfficeData.presentationOffice}</RefNumRES1>
              </CUSOFFPREOFFRES>
          </CC007A>
        )
      }

      trim(convertToXml.buildXml(arrivalNotificationRequest)) mustBe validXml
    }
  }

}
