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
import models.{Trader, TraderWithEori, TraderWithoutEori}
import utils.Format._
import models.messages.request._

class ConvertToSubmissionModel @Inject()(appConfig: AppConfig){

  def convert[A](arrivalNotification: A, eori: String, dateTime: LocalDateTime = LocalDateTime.now(), prefix: String): Either[RequestModelError, RequestModel] = arrivalNotification match {
    case arrivalNotification: NormalNotification => {

      val meta = buildMeta(eori, dateTime, prefix)
      val header = buildHeader(arrivalNotification)
      val traderDestination = buildTrader(arrivalNotification.trader)
      val customsOffice = CustomsOfficeOfPresentation(
        presentationOffice = arrivalNotification.presentationOffice)

      Right(ArrivalNotificationRequest(meta, header, traderDestination, customsOffice))
    }
    case _ =>
      Left(FailedToConvert)
  }

  private def buildMeta(eori: String, dateTime: LocalDateTime, prefix: String): Meta = {
    Meta(
      messageSender = MessageSender(appConfig.env, eori),
      dateOfPreparation = dateFormatted(dateTime),
      timeOfPreparation = timeFormatted(dateTime),
      interchangeControlReference = InterchangeControlReference(prefix, dateTime)
    )
  }

  private def buildHeader(arrivalNotification: NormalNotification): Header = {
    Header(
      movementReferenceNumber = arrivalNotification.movementReferenceNumber,
      customsSubPlace = arrivalNotification.customsSubPlace,
      arrivalNotificationPlace = arrivalNotification.notificationPlace,
      simplifiedProcedureFlag = "0",
      arrivalNotificationDate = dateFormatted(arrivalNotification.notificationDate)
    )
  }

  private def buildTrader(trader: Trader): TraderDestination = trader match {
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

}