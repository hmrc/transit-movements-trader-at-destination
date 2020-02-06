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

import java.time.LocalDate
import java.time.LocalDateTime

import models.messages._
import models.request._
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

  def buildAndEncodeElem[A](value: A, elementTag: String): NodeSeq = value match {
    case result: String => {
      val encodeResult = StringEscapeUtils.escapeXml11(result)
      loadString(s"<$elementTag>$encodeResult</$elementTag>")
    }
    case result: LocalDate         => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case result: Boolean           => loadString(s"<$elementTag>${if (result) 1 else 0}</$elementTag>")
    case result: LanguageCode      => loadString(s"<$elementTag>${result.code}</$elementTag>")
    case result: ProcedureTypeFlag => loadString(s"<$elementTag>${result.code}</$elementTag>")
    case _                         => NodeSeq.Empty
  }

  def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
    case Some(result) => buildAndEncodeElem(result, elementTag)
    case _            => NodeSeq.Empty
  }

  private def createXml(arrivalNotificationRequest: ArrivalNotificationRequest)(implicit dateTime: LocalDateTime): Node = {
    val parentNode: Node = buildParentNode(arrivalNotificationRequest.rootKey, arrivalNotificationRequest.nameSpace)

    val childNodes: NodeSeq = {
      buildMetaNode(arrivalNotificationRequest.meta, arrivalNotificationRequest.messageCode.code) ++
        buildHeaderNode(arrivalNotificationRequest.header, Format.dateFormatted(dateTime)) ++
        arrivalNotificationRequest.traderDestination.toXml ++
        arrivalNotificationRequest.customsOfficeOfPresentation.toXml ++
        buildEnRouteEventsNode(arrivalNotificationRequest)
    }

    addChildrenToRoot(parentNode, childNodes)
  }

  private def buildMetaNode(meta: Meta, messageCode: String)(implicit dateTime: LocalDateTime): NodeSeq =
    buildAndEncodeElem(meta.syntaxIdentifier, "SynIdeMES1") ++
      buildAndEncodeElem(meta.syntaxVersionNumber, "SynVerNumMES2") ++
      buildAndEncodeElem(meta.messageSender.toString, "MesSenMES3") ++
      buildOptionalElem(meta.senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
      buildOptionalElem(meta.recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
      buildAndEncodeElem(meta.messageRecipient, "MesRecMES6") ++
      buildAndEncodeElem(Format.dateFormatted(dateTime), "DatOfPreMES9") ++
      buildAndEncodeElem(Format.timeFormatted(dateTime), "TimOfPreMES10") ++
      buildAndEncodeElem(meta.interchangeControlReference.toString, "IntConRefMES11") ++
      buildOptionalElem(meta.recipientsReferencePassword, "RecRefMES12") ++
      buildOptionalElem(meta.recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
      buildAndEncodeElem(meta.applicationReference, "AppRefMES14") ++
      buildOptionalElem(meta.priority, "PriMES15") ++
      buildOptionalElem(meta.acknowledgementRequest, "AckReqMES16") ++
      buildOptionalElem(meta.communicationsAgreementId, "ComAgrIdMES17") ++
      buildAndEncodeElem(meta.testIndicator, "TesIndMES18") ++
      buildAndEncodeElem(meta.messageIndication, "MesIdeMES19") ++
      buildAndEncodeElem(messageCode, "MesTypMES20") ++
      buildOptionalElem(meta.commonAccessReference, "ComAccRefMES21") ++
      buildOptionalElem(meta.messageSequenceNumber, "MesSeqNumMES22") ++
      buildOptionalElem(meta.firstAndLastTransfer, "FirAndLasTraMES23")

  private def buildHeaderNode(header: Header, arrivalNotificationDate: String): NodeSeq =
    <HEAHEA> {
      buildAndEncodeElem(header.movementReferenceNumber, "DocNumHEA5") ++
      buildOptionalElem(header.customsSubPlace, "CusSubPlaHEA66") ++
      buildAndEncodeElem(header.arrivalNotificationPlace, "ArrNotPlaHEA60") ++
      buildAndEncodeElem(Header.Constants.languageCode, "ArrNotPlaHEA60LNG") ++
      buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocCodHEA62") ++
      buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63") ++
      buildAndEncodeElem(Header.Constants.languageCode, "ArrAgrLocOfGooHEA63LNG") ++
      buildOptionalElem(header.arrivalAgreedLocationOfGoods, "ArrAutLocOfGooHEA65") ++
      buildAndEncodeElem(header.procedureTypeFlag, "SimProFlaHEA132") ++
      buildAndEncodeElem(arrivalNotificationDate, "ArrNotDatHEA141")
      }
    </HEAHEA>

  private def buildEnRouteEventsNode(arrivalNotificationRequest: ArrivalNotificationRequest): NodeSeq = arrivalNotificationRequest.enRouteEvents match {

    case None => NodeSeq.Empty
    case Some(enRouteEvent) =>
      enRouteEvent.map {
        event =>
          <ENROUEVETEV> {
              buildAndEncodeElem(event.place,"PlaTEV10") ++
              buildAndEncodeElem(Header.Constants.languageCode,"PlaTEV10LNG") ++
              buildAndEncodeElem(event.countryCode,"CouTEV13")
            }
            <CTLCTL> {
                buildAndEncodeElem(event.alreadyInNcts,"AlrInNCTCTL29")
              }
            </CTLCTL> {
               buildIncident(event.eventDetails, event.seals)(arrivalNotificationRequest)
            }
          </ENROUEVETEV>
      }
  }

  private def buildSeals(seals: Seq[Seal], languageCode: LanguageCode): NodeSeq = {
    val sealsXml = seals.map {
      seal =>
        <SEAIDSI1>
          <SeaIdeSI11>
            {seal.numberOrMark}
          </SeaIdeSI11>{buildAndEncodeElem(languageCode, "SeaIdeSI11LNG")}
        </SEAIDSI1>
    }

    <SEAINFSF1>
      <SeaNumSF12>
        {seals.size}
      </SeaNumSF12>{sealsXml}
    </SEAINFSF1>
  }

  private def buildIncident(event: EventDetails, sealsOpt: Option[Seq[Seal]])(implicit arrivalNotificationRequest: ArrivalNotificationRequest): NodeSeq = {
    val seals = sealsOpt.fold(NodeSeq.Empty) {
      seal =>
        buildSeals(seal, Header.Constants.languageCode)
    }
    event match {
      case incident: Incident =>
        <INCINC> {
          buildIncidentFlag(incident.information.isDefined) ++
          buildOptionalElem(incident.information, "IncInfINC4") ++
          buildAndEncodeElem(Header.Constants.languageCode, "IncInfINC4LNG") ++
          buildOptionalDate(incident.endorsement.date, "EndDatINC6") ++
          buildOptionalElem(incident.endorsement.authority, "EndAutINC7") ++
          buildAndEncodeElem(Header.Constants.languageCode, "EndAutINC7LNG") ++
          buildOptionalElem(incident.endorsement.place, "EndPlaINC10") ++
          buildAndEncodeElem(Header.Constants.languageCode, "EndPlaINC10LNG") ++
          buildOptionalElem(incident.endorsement.country, "EndCouINC12")
        }
      </INCINC> ++ seals

      case containerTranshipment: ContainerTranshipment =>
        seals ++
          <TRASHP> {
          buildOptionalDate(containerTranshipment.endorsement.date, "EndDatSHP60") ++
          buildOptionalElem(containerTranshipment.endorsement.authority, "EndAutSHP61") ++
          buildAndEncodeElem(Header.Constants.languageCode,"EndAutSHP61LNG") ++
          buildOptionalElem(containerTranshipment.endorsement.place, "EndPlaSHP63") ++
          buildAndEncodeElem(Header.Constants.languageCode,"EndPlaSHP63LNG") ++
          buildOptionalElem(containerTranshipment.endorsement.country, "EndCouSHP65") ++
          buildContainers(Some(containerTranshipment.containers))
        }
      </TRASHP>

      case vehicularTranshipment: VehicularTranshipment =>
        seals ++
          <TRASHP> {
          buildAndEncodeElem(vehicularTranshipment.transportIdentity,"NewTraMeaIdeSHP26") ++
          buildAndEncodeElem(Header.Constants.languageCode,"NewTraMeaIdeSHP26LNG") ++
          buildAndEncodeElem(vehicularTranshipment.transportCountry,"NewTraMeaNatSHP54") ++
          buildOptionalDate(vehicularTranshipment.endorsement.date, "EndDatSHP60") ++
          buildOptionalElem(vehicularTranshipment.endorsement.authority, "EndAutSHP61") ++
          buildAndEncodeElem(Header.Constants.languageCode, "EndAutSHP61LNG") ++
          buildOptionalElem(vehicularTranshipment.endorsement.place, "EndPlaSHP63") ++
          buildAndEncodeElem(Header.Constants.languageCode, "EndPlaSHP63LNG") ++
          buildOptionalElem(vehicularTranshipment.endorsement.country, "EndCouSHP65") ++
          buildContainers(vehicularTranshipment.containers)
        }
      </TRASHP>
    }
  }

  private def buildContainers(containers: Option[Seq[Container]]) = containers match {
    case Some(containers) =>
      containers.map {
        container =>
          <CONNR3> {
              buildAndEncodeElem(container.containerNumber, "ConNumNR31")
            }
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

  private def buildOptionalDate(value: Option[LocalDate], elementTag: String): NodeSeq = value match {
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
