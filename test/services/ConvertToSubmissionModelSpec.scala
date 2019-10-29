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
import generators.ModelGenerators
import models.messages.NormalNotification
import models.messages.request.{ArrivalNotificationRequest, CustomsOfficeOfPresentation, Header, Meta, Root, TraderDestination}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.Injector
import utils.Format

class ConvertToSubmissionModelSpec extends FreeSpec with MustMatchers with GuiceOneAppPerSuite with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  import support.TestConstants._

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToXml = new ConvertToSubmissionModel(appConfig)

  "ConvertToSubmissionModel" - {

//    "convert NormalNotification to ArrivalNotificationXml" ignore {
//
//      val localDateTime: Gen[LocalDateTime] = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)
//
//      forAll(arbitrary[NormalNotification], localDateTime, arbitrary[String]) {
//
//        (notification, localDateTime, authenticatedUserEori) => {
//
//          val timeFormatted = Format.timeFormatted(localDateTime)
//          val dateFormatted = Format.dateFormatted(localDateTime)
//
//          //TODO: this value isn't used by web channel and is not saved within NCTS web database
//          //TODO: has to be unique - websols uses an auto incrementing number from 100000 to 999999
//          val interchangeControlReference = s"WE+${dateFormatted}-1"
//          val messageSender = s"${appConfig.env}+$authenticatedUserEori"
//
//          val metaData = meta(messageSender, dateFormatted, timeFormatted, interchangeControlReference)
//          val headerData = header(notification)
//          val traderDestinationData = traderDestination(notification.trader)
//          val customsOfficeData = customsOffice(notification)
//
//          convertToXml.convert(notification, localDateTime, authenticatedUserEori) mustBe
//            ArrivalNotificationRequest(Root(), metaData, headerData, traderDestinationData, customsOfficeData)
//        }
//
//      }
//    }

    "???" in {
      forAll(arbitrary[NormalNotification], arbitrary[Meta], arbitrary[Header], arbitrary[TraderDestination], arbitrary[CustomsOfficeOfPresentation], arbitrary[String]) {

        (notification, generatedMeta, header, traderDestination, officeOfPresentation, eori) => {

          val meta = generatedMeta.copy(messageSender = s"${appConfig.env}-$eori")

          convertToXml.convert(notification, eori) mustBe ArrivalNotificationRequest(Root(), meta, header, traderDestination, officeOfPresentation)

        }
      }

    }
  }

}