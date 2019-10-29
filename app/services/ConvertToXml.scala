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

import models.messages.xml.ArrivalNotificationXml

import scala.xml.NodeSeq
import scala.xml.XML._
import scala.xml.Utility.trim

class ConvertToXml {

  private[services] def buildNodes(arrivalNotificationXml: ArrivalNotificationXml): NodeSeq = {
        <SynIdeMES1>{arrivalNotificationXml.meta.syntaxIdentifier}</SynIdeMES1>
        <SynVerNumMES2>{arrivalNotificationXml.meta.syntaxVersionNumber}</SynVerNumMES2>
        <MesSenMES3>{arrivalNotificationXml.meta.mesSenMES3}</MesSenMES3> ++
        buildOptionalElem(arrivalNotificationXml.meta.senIdeCodQuaMES4, "SenIdeCodQuaMES4") ++
        buildOptionalElem(arrivalNotificationXml.meta.recIdeCodQuaMES7, "RecIdeCodQuaMES7") ++
        <MesRecMES6>{arrivalNotificationXml.meta.messageRecipient}</MesRecMES6> ++
        buildOptionalElem(arrivalNotificationXml.meta.recRefMES12, "RecRefMES12") ++
        buildOptionalElem(arrivalNotificationXml.meta.recRefQuaMES13, "RecRefQuaMES13") ++
        <AppRefMES14>{arrivalNotificationXml.meta.applicationReference}</AppRefMES14> ++
        buildOptionalElem(arrivalNotificationXml.meta.priMES15, "PriMES15") ++
        buildOptionalElem(arrivalNotificationXml.meta.ackReqMES16, "AckReqMES16") ++
        buildOptionalElem(arrivalNotificationXml.meta.comAgrIdMES17, "ComAgrIdMES17") ++
        <MesIdeMES18>{arrivalNotificationXml.meta.testIndicator}</MesIdeMES18>
        <MesIdeMES19>{arrivalNotificationXml.meta.messageIndication}</MesIdeMES19> ++
        buildOptionalElem(arrivalNotificationXml.meta.comAccRefMES21, "ComAccRefMES21") ++
        buildOptionalElem(arrivalNotificationXml.meta.mesSeqNumMES22, "MesSeqNumMES22") ++
        buildOptionalElem(arrivalNotificationXml.meta.firAndLasTraMES23, "FirAndLasTraMES23") ++
        trim(
          <HEAHEA>
            <DocNumHEA5>{arrivalNotificationXml.header.docNumHEA5}</DocNumHEA5>
            {
              buildOptionalElem(arrivalNotificationXml.header.cusSubPlaHEA66, "CusSubPlaHEA66")
            }
            <ArrNotPlaHEA60>{arrivalNotificationXml.header.arrNotPlaHEA60}</ArrNotPlaHEA60>
            <ArrNotPlaHEA60LNG>{arrivalNotificationXml.header.languageCode}</ArrNotPlaHEA60LNG>
            {
              buildOptionalElem(arrivalNotificationXml.header.arrAgrLocCodHEA62, "ArrAgrLocCodHEA62") ++
              buildOptionalElem(arrivalNotificationXml.header.arrAgrLocOfGooHEA63, "ArrAgrLocOfGooHEA63")
            }
            <ArrAgrLocOfGooHEA63LNG>{arrivalNotificationXml.header.languageCode}</ArrAgrLocOfGooHEA63LNG>
            {
              buildOptionalElem(arrivalNotificationXml.header.arrAutLocOfGooHEA65, "ArrAutLocOfGooHEA65") ++
              buildOptionalElem(arrivalNotificationXml.header.simProFlaHEA132, "SimProFlaHEA132")
            }
            <ArrNotDatHEA141>{arrivalNotificationXml.header.arrNotDatHEA141}</ArrNotDatHEA141>
          </HEAHEA>
        ) ++
        trim(
          <TRADESTRD>
            {
              buildOptionalElem(arrivalNotificationXml.traderDestination.namTRD7String, "NamTRD7") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.strAndNumTRD22, "StrAndNumTRD22") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.posCodTRD23, "PosCodTRD23") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.citTRD24, "CitTRD24") ++
              buildOptionalElem(arrivalNotificationXml.traderDestination.couTRD25, "CouTRD25")
            }
            <NADLNGRD>{arrivalNotificationXml.traderDestination.languageCode}</NADLNGRD>
            {
              buildOptionalElem(arrivalNotificationXml.traderDestination.tintrd59, "TINTRD59")
            }
          </TRADESTRD>
        ) ++
        trim(
          <CUSOFFPREOFFRES>
            <RefNumRES1>{arrivalNotificationXml.customsOfficeOfPresentation.refNumRES1}</RefNumRES1>
          </CUSOFFPREOFFRES>
        )

  }



  private def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
      case Some(result) => loadString(s"<$elementTag>$result</$elementTag>")
      case _ => NodeSeq.Empty
  }

}
