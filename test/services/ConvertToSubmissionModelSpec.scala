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

import config.AppConfig
import generators.MessageGenerators
import models.TraderWithEori
import models.messages.NormalNotification
import models.messages.request.{InterchangeControlReference, _}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.Injector

class ConvertToSubmissionModelSpec extends FreeSpec with MustMatchers with GuiceOneAppPerSuite with MessageGenerators with ScalaCheckDrivenPropertyChecks {

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToSubmissionModel = new ConvertToSubmissionModel(appConfig)

  "ConvertToSubmissionModel" - {

    "must convert NormalNotification to ArrivalNotificationRequest" in {

      def hasEoriWithNormalProcedure()(implicit arrivalNotificationRequest: ArrivalNotificationRequest): Boolean = {
        arrivalNotificationRequest.traderDestination.eori.isDefined &&
          arrivalNotificationRequest.header.simplifiedProcedureFlag.equals("0")
      }

      forAll(arbitrary[ArrivalNotificationRequest]) {

        implicit arrivalNotificationRequest: ArrivalNotificationRequest =>

          whenever(condition = hasEoriWithNormalProcedure) {

            val messageSender: MessageSender = arrivalNotificationRequest.meta.messageSender
            val interchangeControlReference: InterchangeControlReference = arrivalNotificationRequest.meta.interchangeControlReference
            val dateTime = arrivalNotificationRequest.meta.interchangeControlReference.dateTime

            val notification = NormalNotification(
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
                eori = arrivalNotificationRequest.traderDestination.eori.get
              ),
              presentationOffice = arrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice,
              enRouteEvents = Nil
            )

            convertToSubmissionModel.convert(notification, messageSender, interchangeControlReference) mustBe
              Right(arrivalNotificationRequest)
          }
        }
      }
    }

  "must return FailedToConvert when given an invalid request" in {

    import support.InvalidRequestModel

    forAll(arbitrary[MessageSender], arbitrary[InterchangeControlReference]) {

      (messageSender, interchangeControlReference) => {

        val result: Either[RequestModelError, RequestModel] = {
          convertToSubmissionModel.convert(
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
