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

  def buildIncidentFlag(hasIncidentInformation: Boolean): NodeSeq =
    if (hasIncidentInformation) {
      NodeSeq.Empty
    } else {
      <IncFlaINC3>1</IncFlaINC3>
    }

  private def createXml(arrivalNotificationRequest: ArrivalNotificationRequest)(implicit dateTime: LocalDateTime): Node = {
    val parentNode: Node = buildParentNode(arrivalNotificationRequest.rootKey, arrivalNotificationRequest.nameSpace)

    val childNodes: NodeSeq = {
      buildMetaNode(arrivalNotificationRequest.meta, arrivalNotificationRequest.messageCode.code) ++
        arrivalNotificationRequest.header.toXml ++
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
      case incident: Incident                           => incident.toXml ++ seals
      case containerTranshipment: ContainerTranshipment => seals ++ containerTranshipment.toXml
      case vehicularTranshipment: VehicularTranshipment => seals ++ vehicularTranshipment.toXml
    }
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
}

sealed trait XmlBuilderError

object FailedToCreateXml extends XmlBuilderError
