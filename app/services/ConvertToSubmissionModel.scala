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
import models.messages.request._
import models.{Trader, TraderWithEori, TraderWithoutEori}
import utils.Format._

class ConvertToSubmissionModel @Inject()(appConfig: AppConfig){

  /**
    * Conversion layer accepts internal case class
    * converts to xml model
    * returns string of xml
    */

  def convert[A](x: A, eori: String): ArrivalNotificationRequest = x match {
    case arrivalNotification: NormalNotification => {

      val rootNode = Root()
      val meta = buildMeta(eori)
      val header = buildHeader(arrivalNotification)
      val traderDestination = buildTrader(arrivalNotification.trader)
      val customsOffice = CustomsOfficeOfPresentation(
        presentationOffice = arrivalNotification.presentationOffice
      )

      ArrivalNotificationRequest(rootNode, meta, header, traderDestination, customsOffice)
    }
    case _ => {
      throw new RuntimeException
    }

  }

  private def buildMeta(eori: String): Meta = {

    //TODO this value isn't used by web channel and is not saved within NCTS web database
    //TODO: has to be unique - websols uses an auto incrementing number from 100000 to 999999

    val time = LocalDateTime.now()
    val interchangeControlReference = s"WE+${dateFormatted(time)}-1"
    val messageSender = s"${appConfig.env}-$eori"

    Meta(
      messageSender = messageSender,
      senderIdentificationCodeQualifier = None,
      recipientIdentificationCodeQualifier = None,
      dateOfPreparation = dateFormatted(time),
      timeOfPreparation = timeFormatted(time),
      interchangeControlReference = interchangeControlReference,
      recipientsReferencePassword = None,
      recipientsReferencePasswordQualifier = None,
      priority = None,
      acknowledgementRequest = None,
      communicationsAgreementId = None,
      messageType = ArrivalNotificationRequest.messageCode,
      commonAccessReference = None,
      messageSequenceNumber = None,
      firstAndLastTransfer = None)
  }

  private def buildHeader(arrivalNotification: NormalNotification): Header = {
    Header(
      movementReferenceNumber = arrivalNotification.movementReferenceNumber,
      customsSubPlace = arrivalNotification.customsSubPlace,
      arrivalNotificationPlace = arrivalNotification.notificationPlace,
      arrivalNotificationPlaceLNG = None,
      arrivalAgreedLocationOfGoods = None,
      arrivalAgreedLocationOfGoodsLNG = None,
      simplifiedProcedureFlag = Some("0"),
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