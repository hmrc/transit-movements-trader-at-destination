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

import models.messages.request._

import scala.xml.XML._
import scala.xml.{Elem, Node, NodeSeq}

class XmlBuilderService {

  def buildXml(arrivalNotificationRequest: ArrivalNotificationRequest): Node = {

    val rootNode: Node = buildStartRoot(arrivalNotificationRequest.rootKey, arrivalNotificationRequest.nameSpace)

    val childNodes: NodeSeq = {
      buildMetaNode(arrivalNotificationRequest.meta, arrivalNotificationRequest.messageCode) ++
      buildHeaderNode(arrivalNotificationRequest.header) ++
      buildTraderDestinationNode(arrivalNotificationRequest.traderDestination) ++
      buildOfficeOfPresentationNode(arrivalNotificationRequest.customsOfficeOfPresentation)
    }

    val createXml: Node = addChildrenToRoot(childNodes, rootNode)

    createXml
  }

  private def buildStartRoot[A](key: String, nameSpace: Map[String, String]): Node = {

    val concatNameSpace: (String, (String, String)) => String = {
      (accumulatedStrings, keyValue) => s"$accumulatedStrings ${keyValue._1}='${keyValue._2}'"
    }

    val rootWithNameSpace = nameSpace.foldLeft("")(concatNameSpace)

    loadString(s"<$key $rootWithNameSpace></$key>")
  }

  private def addChildrenToRoot(childNodes: NodeSeq, root: Node): Node = {
    Elem(
      root.prefix,
      root.label,
      root.attributes,
      root.scope,
      root.child.isEmpty,
      root.child ++ childNodes: _*
    )
  }

  private def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
    case Some(result) => loadString(s"<$elementTag>$result</$elementTag>")
    case _ => NodeSeq.Empty
  }

  private def buildMetaNode(meta: Meta, messageCode: String): NodeSeq = {
    <SynIdeMES1>{meta.syntaxIdentifier}</SynIdeMES1>
    <SynVerNumMES2>{meta.syntaxVersionNumber}</SynVerNumMES2>
    <MesSenMES3>{meta.messageSender.toString}</MesSenMES3> ++
    buildOptionalElem(meta.senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
    buildOptionalElem(meta.recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
    <MesRecMES6>{meta.messageRecipient}</MesRecMES6> ++
    <DatOfPreMES9>{meta.dateOfPreparation}</DatOfPreMES9> ++
    <TimOfPreMES10>{meta.timeOfPreparation}</TimOfPreMES10> ++
    <IntConRefMES11>{meta.interchangeControlReference.toString}</IntConRefMES11> ++
    buildOptionalElem(meta.recipientsReferencePassword, "RecRefMES12") ++
    buildOptionalElem(meta.recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
    <AppRefMES14>{meta.applicationReference}</AppRefMES14> ++
    buildOptionalElem(meta.priority, "PriMES15") ++
    buildOptionalElem(meta.acknowledgementRequest, "AckReqMES16") ++
    buildOptionalElem(meta.communicationsAgreementId, "ComAgrIdMES17") ++
    <MesIdeMES18>{meta.testIndicator}</MesIdeMES18> ++
    <MesIdeMES19>{meta.messageIndication}</MesIdeMES19> ++
    <MesTypMES20>{messageCode}</MesTypMES20> ++
    buildOptionalElem(meta.commonAccessReference, "ComAccRefMES21") ++
    buildOptionalElem(meta.messageSequenceNumber, "MesSeqNumMES22") ++
    buildOptionalElem(meta.firstAndLastTransfer, "FirAndLasTraMES23")
  }

  private def buildHeaderNode(header: Header): NodeSeq = {
    <HEAHEA>
      <DocNumHEA5>{header.movementReferenceNumber}</DocNumHEA5>
      {
        buildOptionalElem(header.customsSubPlace, "CusSubPlaHEA66")
      }
      <ArrNotPlaHEA60>{header.arrivalNotificationPlace}</ArrNotPlaHEA60>
      <ArrNotPlaHEA60LNG>{header.languageCode}</ArrNotPlaHEA60LNG>
      {
        buildOptionalElem(header.arrivalNotificationPlaceLNG, "ArrAgrLocCodHEA62") ++
        buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63")
      }
      <ArrAgrLocOfGooHEA63LNG>{header.languageCode}</ArrAgrLocOfGooHEA63LNG>
      {
        buildOptionalElem(header.arrivalAgreedLocationOfGoodsLNG, "ArrAutLocOfGooHEA65")
      }
      <SimProFlaHEA132>{header.simplifiedProcedureFlag}</SimProFlaHEA132>
      <ArrNotDatHEA141>{header.arrivalNotificationDate}</ArrNotDatHEA141>
    </HEAHEA>
  }

  private def buildTraderDestinationNode(traderDestination: TraderDestination): NodeSeq = {
    <TRADESTRD>
      {
        buildOptionalElem(traderDestination.name, "NamTRD7") ++
        buildOptionalElem(traderDestination.streetAndNumber, "StrAndNumTRD22") ++
        buildOptionalElem(traderDestination.postCode, "PosCodTRD23") ++
        buildOptionalElem(traderDestination.city, "CitTRD24") ++
        buildOptionalElem(traderDestination.countryCode, "CouTRD25")
      }
      <NADLNGRD>{traderDestination.languageCode}</NADLNGRD>
      {
        buildOptionalElem(traderDestination.eori, "TINTRD59")
      }
    </TRADESTRD>
  }

  private def buildOfficeOfPresentationNode(customsOfficeOfPresentation: CustomsOfficeOfPresentation): NodeSeq = {
    <CUSOFFPREOFFRES>
      <RefNumRES1>{customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
    </CUSOFFPREOFFRES>
  }

}
