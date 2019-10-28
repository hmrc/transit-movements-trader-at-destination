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
import models.messages.xml.{ArrivalNotificationRootNode, ArrivalNotificationXml}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import support.TestConstants._
import utils.Format

import scala.xml.NodeSeq
import scala.xml.Utility.trim

class ConvertToXmlSpec extends FreeSpec with MustMatchers with GuiceOneAppPerSuite {

  val authenticatedUserEori = "NCTS_EU_EXIT"

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToXml = new ConvertToXml()

  "ConvertToXml" - {

    "return correct nodes" in {

      val localDateTime: LocalDateTime = LocalDateTime.now

      val timeFormatted = Format.timeFormatted(localDateTime)
      val dateFormatted = Format.dateFormatted(localDateTime)

      //TODO: this value isn't used by web channel and is not saved within NCTS web database
      //TODO: has to be unique - websols uses an auto incrementing number from 100000 to 999999
      val interchangeControlReference = s"WE+${dateFormatted}-1"
      val messageSender = s"${appConfig.env}+$authenticatedUserEori"


      val notification = normalArrivalNotification(traderWithEori)
      val metaData = meta(messageSender, dateFormatted, timeFormatted, interchangeControlReference)
      val headerData = header(notification)
      val traderDestinationData = traderDestination(notification.trader)
      val customsOfficeData = customsOffice(notification)

      val validXml: NodeSeq = {
            <SynIdeMES1>UNOC</SynIdeMES1>
            <SynVerNumMES2>3</SynVerNumMES2>
            <MesSenMES3>{metaData.mesSenMES3}</MesSenMES3>
            <MesRecMES6>{metaData.messageRecipient}</MesRecMES6>
      }

      convertToXml.buildNodes(ArrivalNotificationXml(ArrivalNotificationRootNode(), metaData, headerData, traderDestinationData, customsOfficeData)) mustBe
        validXml




    }
  }

}
