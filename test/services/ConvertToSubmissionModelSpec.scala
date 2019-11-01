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
import generators.MessageGenerators
import models.messages.NormalNotification
import models.messages.request._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.Injector

class ConvertToSubmissionModelSpec extends FreeSpec with MustMatchers with GuiceOneAppPerSuite with MessageGenerators with ScalaCheckDrivenPropertyChecks {

  import support.TestConstants._

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToSubmissionModel = new ConvertToSubmissionModel(appConfig)

  "ConvertToSubmissionModel" - {

    "must convert NormalNotification to ArrivalNotificationRequest" in {

      val localDateTime: Gen[LocalDateTime] = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)

      forAll(arbitrary[NormalNotification], localDateTime, arbitrary[String], arbitrary[String]) {

        (notification, localDateTime, authenticatedUserEori, prefix) => {

          val metaData = meta(authenticatedUserEori, appConfig.env, prefix, localDateTime)
          val headerData = header(notification)
          val traderDestinationData = traderDestination(notification.trader)
          val customsOfficeData = customsOffice(notification)

          convertToSubmissionModel.convert(notification, authenticatedUserEori, localDateTime, prefix) mustBe
            Right(ArrivalNotificationRequest(metaData, headerData, traderDestinationData, customsOfficeData))
        }
      }
    }

    "must return FailedToConvert when given an invalid request" in {

      import support.InvalidRequestModel

      forAll(arbitrary[String], arbitrary[String]) {

        (authenticatedUserEori, prefix) => {

          val result: Either[RequestModelError, RequestModel] = {
            convertToSubmissionModel.convert(
              arrivalNotification = InvalidRequestModel,
              eori = authenticatedUserEori,
              prefix = prefix
            )
          }

          result mustBe Left(FailedToConvert)
        }
      }
    }
  }

}
