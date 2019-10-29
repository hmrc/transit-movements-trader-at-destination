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

import models.messages.request.ArrivalNotificationRequest

import scala.xml.NodeSeq
import scala.xml.XML._
import scala.xml.Utility.trim

class ConvertToXml {

  private[services] def buildNodes(arrivalNotificationXml: ArrivalNotificationRequest): NodeSeq = {
        <SynIdeMES1>{arrivalNotificationXml.meta.syntaxIdentifier}</SynIdeMES1>
        <SynVerNumMES2>{arrivalNotificationXml.meta.syntaxVersionNumber}</SynVerNumMES2>
        <MesSenMES3>{arrivalNotificationXml.meta.messageSender}</MesSenMES3> ++
        buildOptionalElem(arrivalNotificationXml.meta.senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
        buildOptionalElem(arrivalNotificationXml.meta.recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
        <MesRecMES6>{arrivalNotificationXml.meta.messageRecipient}</MesRecMES6> ++
        buildOptionalElem(arrivalNotificationXml.meta.recipientsReferencePassword, "RecRefMES12") ++
        buildOptionalElem(arrivalNotificationXml.meta.recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
        <AppRefMES14>{arrivalNotificationXml.meta.applicationReference}</AppRefMES14> ++
        buildOptionalElem(arrivalNotificationXml.meta.priority, "PriMES15") ++
        buildOptionalElem(arrivalNotificationXml.meta.acknowledgementRequest, "AckReqMES16") ++
        buildOptionalElem(arrivalNotificationXml.meta.communicationsAgreementId, "ComAgrIdMES17") ++
        <MesIdeMES18>{arrivalNotificationXml.meta.testIndicator}</MesIdeMES18>
        <MesIdeMES19>{arrivalNotificationXml.meta.messageIndication}</MesIdeMES19> ++
        buildOptionalElem(arrivalNotificationXml.meta.commonAccessReference, "ComAccRefMES21") ++
        buildOptionalElem(arrivalNotificationXml.meta.messageSequenceNumber, "MesSeqNumMES22") ++
        buildOptionalElem(arrivalNotificationXml.meta.firstAndLastTransfer, "FirAndLasTraMES23") ++
        trim(
          <HEAHEA>
            <DocNumHEA5>{arrivalNotificationXml.header.movementReferenceNumber}</DocNumHEA5>
            {
              buildOptionalElem(arrivalNotificationXml.header.customsSubPlace, "CusSubPlaHEA66")
            }
            <ArrNotPlaHEA60>{arrivalNotificationXml.header.arrivalNotificationPlace}</ArrNotPlaHEA60>
            <ArrNotPlaHEA60LNG>{arrivalNotificationXml.header.languageCode}</ArrNotPlaHEA60LNG>
            {
              buildOptionalElem(arrivalNotificationXml.header.arrivalNotificationPlaceLNG, "ArrAgrLocCodHEA62") ++
              buildOptionalElem(arrivalNotificationXml.header.arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63")
            }
            <ArrAgrLocOfGooHEA63LNG>{arrivalNotificationXml.header.languageCode}</ArrAgrLocOfGooHEA63LNG>
            {
              buildOptionalElem(arrivalNotificationXml.header.arrivalAgreedLocationOfGoodsLNG, "ArrAutLocOfGooHEA65") ++
              buildOptionalElem(arrivalNotificationXml.header.simplifiedProcedureFlag, "SimProFlaHEA132")
            }
            <ArrNotDatHEA141>{arrivalNotificationXml.header.arrivalNotificationDate}</ArrNotDatHEA141>
          </HEAHEA>
        ) ++
        trim(
          <TRADESTRD>
            {
              buildOptionalElem(arrivalNotificationXml.traderDestination.name, "NamTRD7") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.streetAndNumber, "StrAndNumTRD22") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.postCode, "PosCodTRD23") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.city, "CitTRD24") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.countryCode, "CouTRD25")
            }
            <NADLNGRD>{arrivalNotificationXml.traderDestination.languageCode}</NADLNGRD>
            {
              buildOptionalElem(arrivalNotificationXml.traderDestination.eori, "TINTRD59")
            }
          </TRADESTRD>
        ) ++
        trim(
          <CUSOFFPREOFFRES>
            <RefNumRES1>{arrivalNotificationXml.customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
          </CUSOFFPREOFFRES>
        )

  }



  private def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
      case Some(result) => loadString(s"<$elementTag>$result</$elementTag>")
      case _ => NodeSeq.Empty
  }

}
