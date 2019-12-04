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

import java.time.LocalDate
import java.time.LocalDateTime

import models.ContainerTranshipment
import models.EventDetails
import models.Incident
import models.VehicularTranshipment
import models.messages.request._
import play.api.Logger
import utils.Format

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.XML._

class XmlBuilderService {

  import XmlBuilderService._

  def buildXml(arrivalNotificationRequest: ArrivalNotificationRequest)(implicit dateTime: LocalDateTime): Either[XmlBuilderError, Node] =
    try {

      val xml: Node = createXml(arrivalNotificationRequest)

      Right(xml)

    } catch {
      case e: Exception => {
        logger.info(s"Failed to create Xml with the following exception: $e")
        Left(FailedToCreateXml)
      }
    }

  private def createXml(arrivalNotificationRequest: ArrivalNotificationRequest)(implicit dateTime: LocalDateTime) = {
    val parentNode: Node = buildParentNode(arrivalNotificationRequest.rootKey, arrivalNotificationRequest.nameSpace)

    val childNodes: NodeSeq = {
      buildMetaNode(arrivalNotificationRequest.meta, arrivalNotificationRequest.messageCode.code) ++
        buildHeaderNode(arrivalNotificationRequest.header, Format.dateFormatted(dateTime)) ++
        buildTraderDestinationNode(arrivalNotificationRequest.traderDestination) ++
        buildOfficeOfPresentationNode(arrivalNotificationRequest.customsOfficeOfPresentation) ++
        buildEnRouteEventsNode(arrivalNotificationRequest)
    }

    addChildrenToRoot(parentNode, childNodes)
  }

  private def buildMetaNode(meta: Meta, messageCode: String)(implicit dateTime: LocalDateTime): NodeSeq =
    <SynIdeMES1>{meta.syntaxIdentifier}</SynIdeMES1>
      <SynVerNumMES2>{meta.syntaxVersionNumber}</SynVerNumMES2>
      <MesSenMES3>{meta.messageSender.toString}</MesSenMES3> ++
      buildOptionalElem(meta.senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
      buildOptionalElem(meta.recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
      <MesRecMES6>{meta.messageRecipient}</MesRecMES6> ++
      <DatOfPreMES9>{Format.dateFormatted(dateTime)}</DatOfPreMES9> ++
      <TimOfPreMES10>{Format.timeFormatted(dateTime)}</TimOfPreMES10> ++
      <IntConRefMES11>{meta.interchangeControlReference.toString}</IntConRefMES11> ++
      buildOptionalElem(meta.recipientsReferencePassword, "RecRefMES12") ++
      buildOptionalElem(meta.recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
      <AppRefMES14>{meta.applicationReference}</AppRefMES14> ++
      buildOptionalElem(meta.priority, "PriMES15") ++
      buildOptionalElem(meta.acknowledgementRequest, "AckReqMES16") ++
      buildOptionalElem(meta.communicationsAgreementId, "ComAgrIdMES17") ++
      <TesIndMES18>{meta.testIndicator}</TesIndMES18> ++
      <MesIdeMES19>{meta.messageIndication}</MesIdeMES19> ++
      <MesTypMES20>{messageCode}</MesTypMES20> ++
      buildOptionalElem(meta.commonAccessReference, "ComAccRefMES21") ++
      buildOptionalElem(meta.messageSequenceNumber, "MesSeqNumMES22") ++
      buildOptionalElem(meta.firstAndLastTransfer, "FirAndLasTraMES23")

  private def buildHeaderNode(header: Header, arrivalNotificationDate: String): NodeSeq =
    <HEAHEA>
      <DocNumHEA5>{header.movementReferenceNumber}</DocNumHEA5>
      {buildOptionalElem(header.customsSubPlace, "CusSubPlaHEA66")}
      <ArrNotPlaHEA60>{header.arrivalNotificationPlace}</ArrNotPlaHEA60>
      <ArrNotPlaHEA60LNG>{header.languageCode}</ArrNotPlaHEA60LNG>
      {buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocCodHEA62") ++
      buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63")}
      <ArrAgrLocOfGooHEA63LNG>{header.languageCode}</ArrAgrLocOfGooHEA63LNG>
      {buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAutLocOfGooHEA65")}
      <SimProFlaHEA132>{header.simplifiedProcedureFlag}</SimProFlaHEA132>
      <ArrNotDatHEA141>{arrivalNotificationDate}</ArrNotDatHEA141>
    </HEAHEA>

  private def buildTraderDestinationNode(traderDestination: TraderDestination): NodeSeq =
    <TRADESTRD>
      {buildOptionalElem(traderDestination.name, "NamTRD7") ++
      buildOptionalElem(traderDestination.streetAndNumber, "StrAndNumTRD22") ++
      buildOptionalElem(traderDestination.postCode, "PosCodTRD23") ++
      buildOptionalElem(traderDestination.city, "CitTRD24") ++
      buildOptionalElem(traderDestination.countryCode, "CouTRD25")}
      <NADLNGRD>{traderDestination.languageCode}</NADLNGRD>
      {buildOptionalElem(traderDestination.eori, "TINTRD59")}
    </TRADESTRD>

  private def buildOfficeOfPresentationNode(customsOfficeOfPresentation: CustomsOfficeOfPresentation): NodeSeq =
    <CUSOFFPREOFFRES>
      <RefNumRES1>{customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
    </CUSOFFPREOFFRES>

  private def buildEnRouteEventsNode(arrivalNotificationRequest: ArrivalNotificationRequest): NodeSeq = arrivalNotificationRequest.enRouteEvents match {

    case None => NodeSeq.Empty
    case Some(enRouteEvent) =>
      enRouteEvent.map {
        event =>
          <ENROUEVETEV>
            <PlaTEV10>{event.place}</PlaTEV10>
            <PlaTEV10LNG>{arrivalNotificationRequest.header.languageCode}</PlaTEV10LNG>
            <CouTEV13>{event.countryCode}</CouTEV13>
            <CTLCTL>
              <AlrInNCTCTL29>{if (event.alreadyInNcts) 1 else 0}</AlrInNCTCTL29>
            </CTLCTL>
            {buildIncident(event.eventDetails)(arrivalNotificationRequest)}
          </ENROUEVETEV>
      }
  }

  private def buildIncident(event: EventDetails)(implicit arrivalNotificationRequest: ArrivalNotificationRequest): NodeSeq = event match {
    case incident: Incident => {
      <INCINC>
        {buildIncidentFlag(incident.information.isDefined)}
        {buildOptionalElem(incident.information, "IncInfINC4")}
        <IncInfINC4LNG>{arrivalNotificationRequest.header.languageCode}</IncInfINC4LNG>
        {buildOptionalElem(incident.endorsement.date, "EndDatINC6")}
        {buildOptionalElem(incident.endorsement.authority, "EndAutINC7")}
        <EndAutINC7LNG>{arrivalNotificationRequest.header.languageCode}</EndAutINC7LNG>
        {buildOptionalElem(incident.endorsement.place, "EndPlaINC10")}
        <EndPlaINC10LNG>{arrivalNotificationRequest.header.languageCode}</EndPlaINC10LNG>
        {buildOptionalElem(incident.endorsement.country, "EndCouINC12")}
      </INCINC>
    }
    case containerTranshipment: ContainerTranshipment => {
      <TRASHP>
        {buildOptionalElem(containerTranshipment.endorsement.date, "EndDatSHP60")}
        {buildOptionalElem(containerTranshipment.endorsement.authority, "EndAutSHP61")}
        <EndAutSHP61LNG>{arrivalNotificationRequest.header.languageCode}</EndAutSHP61LNG>
        {buildOptionalElem(containerTranshipment.endorsement.place, "EndPlaSHP63")}
        <EndPlaSHP63LNG>{arrivalNotificationRequest.header.languageCode}</EndPlaSHP63LNG>
        {buildOptionalElem(containerTranshipment.endorsement.country, "EndCouSHP65")}
        {
          containerTranshipment.containers.map {
            container =>
              <CONNR3>
                <ConNumNR31>{container}</ConNumNR31>
              </CONNR3>
          }
        }
      </TRASHP>
    }
    case vehicularTranshipment: VehicularTranshipment =>
      <TRASHP>
        <NewTraMeaIdeSHP26>{vehicularTranshipment.transportIdentity}</NewTraMeaIdeSHP26>
        <NewTraMeaIdeSHP26LNG>{arrivalNotificationRequest.header.languageCode}</NewTraMeaIdeSHP26LNG>
        <NewTraMeaNatSHP54>{vehicularTranshipment.transportCountry}</NewTraMeaNatSHP54>
        {buildOptionalElem(vehicularTranshipment.endorsement.date, "EndDatSHP60")}
        {buildOptionalElem(vehicularTranshipment.endorsement.authority, "EndAutSHP61")}
        <EndAutSHP61LNG>{arrivalNotificationRequest.header.languageCode}</EndAutSHP61LNG>
        {buildOptionalElem(vehicularTranshipment.endorsement.place, "EndPlaSHP63")}
        <EndPlaSHP63LNG>{arrivalNotificationRequest.header.languageCode}</EndPlaSHP63LNG>
        {buildOptionalElem(vehicularTranshipment.endorsement.country, "EndCouSHP65")}
        {
        vehicularTranshipment.containers.map {
          container =>
              <CONNR3>
              <ConNumNR31>{container}</ConNumNR31>
            </CONNR3>
           }
        }
      </TRASHP>
  }
}

object XmlBuilderService {

  private val logger = Logger(getClass)

  private def buildParentNode[A](key: String, nameSpace: Map[String, String]): Node = {

    val concatNameSpace: (String, (String, String)) => String = {
      (accumulatedStrings, keyValue) =>
        s"$accumulatedStrings ${keyValue._1}='${keyValue._2}'"
    }

    val rootWithNameSpace = nameSpace.foldLeft("")(concatNameSpace)

    loadString(s"<$key $rootWithNameSpace></$key>")
  }

  private def addChildrenToRoot(root: Node, childNodes: NodeSeq): Node =
    Elem(
      root.prefix,
      root.label,
      root.attributes,
      root.scope,
      root.child.isEmpty,
      root.child ++ childNodes: _*
    )

  private def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
    case Some(result: String)    => loadString(s"<$elementTag>$result</$elementTag>")
    case Some(result: LocalDate) => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case _                       => NodeSeq.Empty
  }

  private def buildIncidentFlag(hasIncidentInformation: Boolean): NodeSeq = hasIncidentInformation match {
    case false => <IncFlaINC3>1</IncFlaINC3>
    case true  => NodeSeq.Empty
  }
}

sealed trait XmlBuilderError

object FailedToCreateXml extends XmlBuilderError
