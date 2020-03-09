/*
 * Copyright 2020 HM Revenue & Customs
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

import com.google.inject.Inject
import config.AppConfig
import models.messages.EnRouteEvent
import models.messages.NormalNotificationMessage
import models.messages.Trader
import models.messages.TraderWithEori
import models.messages.TraderWithoutEori
import models.request._

class SubmissionModelService @Inject()(appConfig: AppConfig) {

  def convertToSubmissionModel[A](arrivalNotification: A,
                                  messageSender: MessageSender,
                                  interchangeControlReference: InterchangeControlReference): Either[ModelConversionError, ArrivalNotificationRequest] =
    arrivalNotification match {
      case arrivalNotification: NormalNotificationMessage =>
        val meta                                     = buildMeta(messageSender: MessageSender, interchangeControlReference)
        val header                                   = buildHeader(arrivalNotification, NormalProcedureFlag)
        val traderDestination                        = buildTrader(arrivalNotification.trader)
        val customsOffice                            = CustomsOfficeOfPresentation(presentationOffice = arrivalNotification.presentationOffice)
        val enRouteEvents: Option[Seq[EnRouteEvent]] = arrivalNotification.enRouteEvents

        Right(ArrivalNotificationRequest(meta, header, traderDestination, customsOffice, enRouteEvents))
      case _ =>
        Left(FailedToConvertModel)
    }

  private def buildMeta(messageSender: MessageSender, interchangeControlReference: InterchangeControlReference): Meta =
    Meta(messageSender = messageSender, interchangeControlReference = interchangeControlReference)

  private def buildHeader(arrivalNotification: NormalNotificationMessage, procedureTypeFlag: ProcedureTypeFlag): Header =
    Header(
      movementReferenceNumber = arrivalNotification.movementReferenceNumber,
      customsSubPlace = arrivalNotification.customsSubPlace,
      arrivalNotificationPlace = arrivalNotification.notificationPlace,
      procedureTypeFlag = procedureTypeFlag,
      notificationDate = arrivalNotification.notificationDate
    )

  private def buildTrader(trader: Trader): TraderDestination = trader match {
    case traderWithEori: TraderWithEori =>
      TraderDestination(
        name = traderWithEori.name,
        streetAndNumber = traderWithEori.streetAndNumber,
        postCode = traderWithEori.postCode,
        city = traderWithEori.city,
        countryCode = traderWithEori.countryCode,
        eori = Some(traderWithEori.eori)
      )
    case traderWithoutEori: TraderWithoutEori =>
      TraderDestination(
        name = Some(traderWithoutEori.name),
        streetAndNumber = Some(traderWithoutEori.streetAndNumber),
        postCode = Some(traderWithoutEori.postCode),
        city = Some(traderWithoutEori.city),
        countryCode = Some(traderWithoutEori.countryCode),
        eori = None
      )
  }

}

sealed trait ModelConversionError

object FailedToConvertModel extends ModelConversionError
