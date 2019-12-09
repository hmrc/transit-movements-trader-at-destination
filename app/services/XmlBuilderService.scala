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
import play.twirl.api.utils.StringEscapeUtils
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

  private def createXml(arrivalNotificationRequest: ArrivalNotificationRequest)(implicit dateTime: LocalDateTime): Node = {
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
    buildAndEncodeElem(meta.syntaxIdentifier, "SynIdeMES1") ++
      buildAndEncodeElem(meta.syntaxVersionNumber, "SynVerNumMES2") ++
      buildAndEncodeElem(meta.messageSender.toString, "MesSenMES3") ++
      buildAndEncodeOptionalElem(meta.senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
      buildAndEncodeOptionalElem(meta.recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
      buildAndEncodeElem(meta.messageRecipient, "MesRecMES6") ++
      buildAndEncodeElem(Format.dateFormatted(dateTime), "DatOfPreMES9") ++
      buildAndEncodeElem(Format.timeFormatted(dateTime), "TimOfPreMES10") ++
      buildAndEncodeElem(meta.interchangeControlReference, "IntConRefMES11") ++
      buildAndEncodeOptionalElem(meta.recipientsReferencePassword, "RecRefMES12") ++
      buildAndEncodeOptionalElem(meta.recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
      buildAndEncodeElem(meta.applicationReference, "AppRefMES14") ++
      buildAndEncodeOptionalElem(meta.priority, "PriMES15") ++
      buildAndEncodeOptionalElem(meta.acknowledgementRequest, "AckReqMES16") ++
      buildAndEncodeOptionalElem(meta.communicationsAgreementId, "ComAgrIdMES17") ++
      buildAndEncodeElem(meta.testIndicator, "TesIndMES18") ++
      buildAndEncodeElem(meta.messageIndication, "MesIdeMES19") ++
      buildAndEncodeElem(messageCode, "MesTypMES20") ++
      buildAndEncodeOptionalElem(meta.commonAccessReference, "ComAccRefMES21") ++
      buildAndEncodeOptionalElem(meta.messageSequenceNumber, "MesSeqNumMES22") ++
      buildAndEncodeOptionalElem(meta.firstAndLastTransfer, "FirAndLasTraMES23")

  private def buildHeaderNode(header: Header, arrivalNotificationDate: String): NodeSeq =
    <HEAHEA> {
      buildAndEncodeElem(header.movementReferenceNumber, "DocNumHEA5")
      buildAndEncodeOptionalElem(header.customsSubPlace, "CusSubPlaHEA66") ++
      buildAndEncodeElem(header.arrivalNotificationPlace, "ArrNotPlaHEA60") ++
      buildAndEncodeElem(header.languageCode, "ArrNotPlaHEA60LNG") ++
      buildAndEncodeOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocCodHEA62") ++
      buildAndEncodeOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63") ++
      buildAndEncodeElem(header.languageCode, "ArrAgrLocOfGooHEA63LNG") ++
      buildAndEncodeOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAutLocOfGooHEA65") ++
      buildAndEncodeElem(header.simplifiedProcedureFlag, "SimProFlaHEA132") ++
      buildAndEncodeElem(arrivalNotificationDate, "ArrNotDatHEA141")
      }
    </HEAHEA>

  private def buildTraderDestinationNode(traderDestination: TraderDestination): NodeSeq =
    <TRADESTRD>{
      buildAndEncodeOptionalElem(traderDestination.name, "NamTRD7") ++
      buildAndEncodeOptionalElem(traderDestination.streetAndNumber, "StrAndNumTRD22") ++
      buildAndEncodeOptionalElem(traderDestination.postCode, "PosCodTRD23") ++
      buildAndEncodeOptionalElem(traderDestination.city, "CitTRD24") ++
      buildAndEncodeOptionalElem(traderDestination.countryCode, "CouTRD25") ++
      buildAndEncodeElem(traderDestination.languageCode, "NADLNGRD") ++
      buildAndEncodeOptionalElem(traderDestination.eori, "TINTRD59")
      }
    </TRADESTRD>

  private def buildOfficeOfPresentationNode(customsOfficeOfPresentation: CustomsOfficeOfPresentation): NodeSeq =
    <CUSOFFPREOFFRES>
      {buildAndEncodeElem(customsOfficeOfPresentation.presentationOffice, "RefNumRES1")}
    </CUSOFFPREOFFRES>

  private def buildEnRouteEventsNode(arrivalNotificationRequest: ArrivalNotificationRequest): NodeSeq =
    arrivalNotificationRequest.enRouteEvents match {

      case None => NodeSeq.Empty
      case Some(enRouteEvent) =>
        enRouteEvent.map {
          event =>
            <ENROUEVETEV> {
            buildAndEncodeElem(event.place, "PlaTEV10") ++
            buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "PlaTEV10LNG") ++
            buildAndEncodeElem(event.countryCode, "CouTEV13")
            }
            <CTLCTL>
              {buildAndEncodeElem(event.alreadyInNcts, "AlrInNCTCTL29")}
            </CTLCTL>
            {buildIncident(event.eventDetails)(arrivalNotificationRequest)}
          </ENROUEVETEV>
        }
    }

  private def buildIncident(event: EventDetails)(implicit arrivalNotificationRequest: ArrivalNotificationRequest): NodeSeq = event match {
    case incident: Incident => {
      <INCINC> {
        buildIncidentFlag(incident.information.isDefined) ++
        buildAndEncodeOptionalElem(incident.information, "IncInfINC4") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "IncInfINC4LNG") ++
        buildAndEncodeOptionalElem(incident.endorsement.date, "EndDatINC6") ++
        buildAndEncodeOptionalElem(incident.endorsement.authority, "EndAutINC7") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "EndAutINC7LNG") ++
        buildAndEncodeOptionalElem(incident.endorsement.place, "EndPlaINC10") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "EndPlaINC10LNG") ++
        buildAndEncodeOptionalElem(incident.endorsement.country, "EndCouINC12")
        }
      </INCINC>
    }
    case containerTranshipment: ContainerTranshipment => {
      <TRASHP> {
        buildAndEncodeOptionalElem(containerTranshipment.endorsement.date, "EndDatSHP60") ++
        buildAndEncodeOptionalElem(containerTranshipment.endorsement.authority, "EndAutSHP61") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "EndAutSHP61LNG") ++
        buildAndEncodeOptionalElem(containerTranshipment.endorsement.place, "EndPlaSHP63") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "EndPlaSHP63LNG") ++
        buildAndEncodeOptionalElem(containerTranshipment.endorsement.country, "EndCouSHP65") ++
        buildContainers(Some(containerTranshipment.containers))
        }
      </TRASHP>
    }
    case vehicularTranshipment: VehicularTranshipment =>
      <TRASHP> {
        buildAndEncodeElem(vehicularTranshipment.transportIdentity, "NewTraMeaIdeSHP26") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "NewTraMeaIdeSHP26LNG") ++
        buildAndEncodeElem(vehicularTranshipment.transportCountry, "NewTraMeaNatSHP54") ++
        buildAndEncodeOptionalElem(vehicularTranshipment.endorsement.date, "EndDatSHP60") ++
        buildAndEncodeOptionalElem(vehicularTranshipment.endorsement.authority, "EndAutSHP61") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "EndAutSHP61LNG") ++
        buildAndEncodeOptionalElem(vehicularTranshipment.endorsement.place, "EndPlaSHP63") ++
        buildAndEncodeElem(arrivalNotificationRequest.header.languageCode, "EndPlaSHP63LNG") ++
        buildAndEncodeOptionalElem(vehicularTranshipment.endorsement.country, "EndCouSHP65") ++
        buildContainers(vehicularTranshipment.containers)
        }
      </TRASHP>
  }

  private def buildContainers(containers: Option[Seq[String]]) = containers match {
    case Some(containers) =>
      containers.map {
        container =>
          <CONNR3>
            {buildAndEncodeElem(container, "ConNumNR31")}
          </CONNR3>
      }
    case _ => NodeSeq.Empty
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
    Elem(root.prefix, root.label, root.attributes, root.scope, root.child.isEmpty, root.child ++ childNodes: _*)

  private def buildAndEncodeOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
    case Some(result: String) => {
      val encodeResult = StringEscapeUtils.escapeXml11(result)
      loadString(s"<$elementTag>$encodeResult</$elementTag>")
    }
    case Some(result: LocalDate) => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case _                       => NodeSeq.Empty
  }

  private def buildAndEncodeElem[A](value: A, elementTag: String): NodeSeq = value match {
    case result: String =>
      val encodeResult = StringEscapeUtils.escapeXml11(result)
      loadString(s"<$elementTag>$encodeResult</$elementTag>")
    case result: LocalDate => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case result: Boolean   => loadString(s"<$elementTag>${if (result) 1 else 0}</$elementTag>")
    case _                 => NodeSeq.Empty
  }

  private def buildIncidentFlag(hasIncidentInformation: Boolean): NodeSeq = hasIncidentInformation match {
    case false => <IncFlaINC3>1</IncFlaINC3>
    case true  => NodeSeq.Empty
  }
}

sealed trait XmlBuilderError

object FailedToCreateXml extends XmlBuilderError
