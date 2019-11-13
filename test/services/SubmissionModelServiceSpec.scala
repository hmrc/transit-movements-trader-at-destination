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
import models.{TraderWithEori, TraderWithoutEori}
import models.messages.NormalNotification
import models.messages.request.{InterchangeControlReference, _}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.Injector

class SubmissionModelServiceSpec extends FreeSpec
  with MustMatchers with GuiceOneAppPerSuite with MessageGenerators with ScalaCheckDrivenPropertyChecks with OptionValues {

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToSubmissionModel = new SubmissionModelService(appConfig)

  "SubmissionModelService" - {

    "must convert NormalNotification to ArrivalNotificationRequest for traders with Eori" in {

      val notifications: Gen[(ArrivalNotificationRequest, NormalNotification)] = {
        for {
          arrivalNotificationRequest <- arbitraryArrivalNotificationRequestWithEori
          dateTime <- dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)
        } yield {

          val normalNotification: NormalNotification = {
            NormalNotification(
              movementReferenceNumber = arrivalNotificationRequest.header.movementReferenceNumber,
              notificationPlace = arrivalNotificationRequest.header.arrivalNotificationPlace,
              notificationDate = dateTime.toLocalDate,
              customsSubPlace = arrivalNotificationRequest.header.customsSubPlace,
              trader = TraderWithEori(
                name = arrivalNotificationRequest.traderDestination.name,
                streetAndNumber = arrivalNotificationRequest.traderDestination.streetAndNumber,
                postCode = arrivalNotificationRequest.traderDestination.postCode,
                city = arrivalNotificationRequest.traderDestination.city,
                countryCode = arrivalNotificationRequest.traderDestination.countryCode,
                eori = arrivalNotificationRequest.traderDestination.eori.value
              ),
              presentationOffice = arrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice,
              enRouteEvents = Nil
            )
          }

          (arrivalNotificationRequest, normalNotification)
        }
      }

      forAll(notifications) {

        case (arrivalNotificationRequest, normalNotification) =>

          val messageSender: MessageSender = arrivalNotificationRequest.meta.messageSender
          val interchangeControlReference: InterchangeControlReference = arrivalNotificationRequest.meta.interchangeControlReference

          convertToSubmissionModel.convertFromArrivalNotification(normalNotification, messageSender, interchangeControlReference) mustBe
            Right(arrivalNotificationRequest)
      }
    }
  }

  "must convert NormalNotification to ArrivalNotificationRequest for traders without Eori" in {

    val notifications: Gen[(ArrivalNotificationRequest, NormalNotification)] = {
      for {
        arrivalNotificationRequest <- arbitraryArrivalNotificationRequestWithoutEori
        dateTime <- dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)
      } yield {

        val normalNotification: NormalNotification = {
          NormalNotification(
            movementReferenceNumber = arrivalNotificationRequest.header.movementReferenceNumber,
            notificationPlace = arrivalNotificationRequest.header.arrivalNotificationPlace,
            notificationDate = dateTime.toLocalDate,
            customsSubPlace = arrivalNotificationRequest.header.customsSubPlace,
            trader = TraderWithoutEori(
              name = arrivalNotificationRequest.traderDestination.name.value,
              streetAndNumber = arrivalNotificationRequest.traderDestination.streetAndNumber.value,
              postCode = arrivalNotificationRequest.traderDestination.postCode.value,
              city = arrivalNotificationRequest.traderDestination.city.value,
              countryCode = arrivalNotificationRequest.traderDestination.countryCode.value
            ),
            presentationOffice = arrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice,
            enRouteEvents = Nil
          )
        }

        (arrivalNotificationRequest, normalNotification)
      }
    }

    forAll(notifications) {

      case (arrivalNotificationRequest, normalNotification) =>

        val messageSender: MessageSender = arrivalNotificationRequest.meta.messageSender
        val interchangeControlReference: InterchangeControlReference = arrivalNotificationRequest.meta.interchangeControlReference

        convertToSubmissionModel.convertFromArrivalNotification(normalNotification, messageSender, interchangeControlReference) mustBe
          Right(arrivalNotificationRequest)
    }
  }


    "must return FailedToConvert when given an invalid request" in {

      import support.InvalidRequestModel

      forAll(arbitrary[MessageSender], arbitrary[InterchangeControlReference]) {

        (messageSender, interchangeControlReference) => {

          val result: Either[RequestModelError, RequestModel] = {
            convertToSubmissionModel.convertFromArrivalNotification(
              arrivalNotification = InvalidRequestModel,
              messageSender = messageSender,
              interchangeControlReference = interchangeControlReference
            )
          }

          result mustBe Left(FailedToConvert)
        }
      }
    }


}
