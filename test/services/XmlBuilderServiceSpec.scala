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

import generators.MessageGenerators
import models.messages._
import models.request._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.twirl.api.utils.StringEscapeUtils
import utils.Format

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility.trim
import scala.xml.XML.loadString

class XmlBuilderServiceSpec
    extends FreeSpec
    with MustMatchers
    with GuiceOneAppPerSuite
    with MessageGenerators
    with ScalaCheckDrivenPropertyChecks
    with OptionValues {

  private val convertToXml = new XmlBuilderService()

  private val dateTime: LocalDateTime = LocalDateTime.now()
  private val dateOfPreparation       = Format.dateFormatted(dateTime)
  private val timeOfPreparation       = Format.timeFormatted(dateTime)

  private val minimalArrivalNotificationRequest: ArrivalNotificationRequest =
    ArrivalNotificationRequest(
      meta = Meta(
        MessageSender("LOCAL", "EORI&123"),
        InterchangeControlReference("2019", 1)
      ),
      header = Header("MovementReferenceNumber", None, "arrivalNotificationPlace", None, NormalProcedureFlag),
      traderDestination = TraderDestination(None, None, None, None, None, None),
      customsOfficeOfPresentation = CustomsOfficeOfPresentation("PresentationOffice"),
      enRouteEvents = None
    )

  private val minimalValidXml: Node = {
    trim(
      <CC007A>
        <SynIdeMES1>{minimalArrivalNotificationRequest.syntaxIdentifier}</SynIdeMES1>
        <SynVerNumMES2>{minimalArrivalNotificationRequest.meta.syntaxVersionNumber}</SynVerNumMES2>
        <MesSenMES3>{minimalArrivalNotificationRequest.meta.messageSender.toString}</MesSenMES3>
        <MesRecMES6>{minimalArrivalNotificationRequest.meta.messageRecipient}</MesRecMES6>
        <DatOfPreMES9>{dateOfPreparation}</DatOfPreMES9>
        <TimOfPreMES10>{timeOfPreparation}</TimOfPreMES10>
        <IntConRefMES11>{minimalArrivalNotificationRequest.meta.interchangeControlReference.toString}</IntConRefMES11>
        <AppRefMES14>{minimalArrivalNotificationRequest.meta.applicationReference}</AppRefMES14>
        <TesIndMES18>{minimalArrivalNotificationRequest.meta.testIndicator}</TesIndMES18>
        <MesIdeMES19>{minimalArrivalNotificationRequest.meta.messageIndication}</MesIdeMES19>
        <MesTypMES20>{minimalArrivalNotificationRequest.messageCode.code}</MesTypMES20>
        <HEAHEA>
          <DocNumHEA5>{minimalArrivalNotificationRequest.header.movementReferenceNumber}</DocNumHEA5>
          <ArrNotPlaHEA60>{minimalArrivalNotificationRequest.header.arrivalNotificationPlace}</ArrNotPlaHEA60>
          <ArrNotPlaHEA60LNG>{Header.Constants.languageCode.code}</ArrNotPlaHEA60LNG>
          <ArrAgrLocOfGooHEA63LNG>{Header.Constants.languageCode.code}</ArrAgrLocOfGooHEA63LNG>
          <SimProFlaHEA132>{minimalArrivalNotificationRequest.header.procedureTypeFlag.code}</SimProFlaHEA132>
          <ArrNotDatHEA141>{dateOfPreparation}</ArrNotDatHEA141>
        </HEAHEA>
        <TRADESTRD>
          <NADLNGRD>{Header.Constants.languageCode.code}</NADLNGRD>
        </TRADESTRD>
        <CUSOFFPREOFFRES>
          <RefNumRES1>{minimalArrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
        </CUSOFFPREOFFRES>
      </CC007A>
    )
  }

  "XmlBuilderService" - {
    "must return correct nodes" in {
      val localDateTime: Gen[LocalDateTime] = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)

      forAll(arbitrary[ArrivalNotificationRequest](arbitraryArrivalNotificationRequest), localDateTime) {

        (arrivalNotificationRequest, genDateTime) =>
          whenever(hasEoriWithNormalProcedure()(arrivalNotificationRequest)) {

            val genDateOfPreparation = Format.dateFormatted(genDateTime)
            val genTimeOfPreparation = Format.timeFormatted(genDateTime)

            val validXml: Node = {
              trim(
                <CC007A> {
                    buildAndEncodeElem(arrivalNotificationRequest.meta.syntaxIdentifier, "SynIdeMES1") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.syntaxVersionNumber, "SynVerNumMES2") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.messageSender.toString, "MesSenMES3") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.messageRecipient, "MesRecMES6") ++
                    buildAndEncodeElem(genDateOfPreparation, "DatOfPreMES9") ++
                    buildAndEncodeElem(genTimeOfPreparation, "TimOfPreMES10") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.interchangeControlReference.toString, "IntConRefMES11") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.recipientsReferencePassword, "RecRefMES12") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.applicationReference, "AppRefMES14") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.priority, "PriMES15") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.acknowledgementRequest, "AckReqMES16") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.communicationsAgreementId, "ComAgrIdMES17") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.testIndicator, "TesIndMES18") ++
                    buildAndEncodeElem(arrivalNotificationRequest.meta.messageIndication, "MesIdeMES19") ++
                    buildAndEncodeElem(arrivalNotificationRequest.messageCode.code, "MesTypMES20") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.commonAccessReference, "ComAccRefMES21") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.messageSequenceNumber, "MesSeqNumMES22") ++
                    buildOptionalElem(arrivalNotificationRequest.meta.firstAndLastTransfer, "FirAndLasTraMES23")
                  }
                  <HEAHEA> {
                    buildAndEncodeElem(arrivalNotificationRequest.header.movementReferenceNumber, "DocNumHEA5") ++
                    buildOptionalElem(arrivalNotificationRequest.header.customsSubPlace, "CusSubPlaHEA66") ++
                    buildAndEncodeElem(arrivalNotificationRequest.header.arrivalNotificationPlace, "ArrNotPlaHEA60") ++
                    buildAndEncodeElem(Header.Constants.languageCode, "ArrNotPlaHEA60LNG") ++
                    buildOptionalElem(arrivalNotificationRequest.header.arrivalAgreedLocationOfGoods, "ArrAgrLocCodHEA62") ++
                    buildOptionalElem(arrivalNotificationRequest.header.arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63") ++
                    buildAndEncodeElem(Header.Constants.languageCode, "ArrAgrLocOfGooHEA63LNG") ++
                    buildOptionalElem(arrivalNotificationRequest.header.arrivalAgreedLocationOfGoods, "ArrAutLocOfGooHEA65") ++
                    buildAndEncodeElem(arrivalNotificationRequest.header.procedureTypeFlag, "SimProFlaHEA132") ++
                    buildAndEncodeElem(genDateOfPreparation, "ArrNotDatHEA141")
                  }
                  </HEAHEA>
                  <TRADESTRD> {
                    buildOptionalElem(arrivalNotificationRequest.traderDestination.name, "NamTRD7") ++
                    buildOptionalElem(arrivalNotificationRequest.traderDestination.streetAndNumber, "StrAndNumTRD22") ++
                    buildOptionalElem(arrivalNotificationRequest.traderDestination.postCode, "PosCodTRD23") ++
                    buildOptionalElem(arrivalNotificationRequest.traderDestination.city, "CitTRD24") ++
                    buildOptionalElem(arrivalNotificationRequest.traderDestination.countryCode, "CouTRD25") ++
                    buildAndEncodeElem(TraderDestination.Constants.languageCode, "NADLNGRD") ++
                    buildOptionalElem(arrivalNotificationRequest.traderDestination.eori, "TINTRD59")
                    }
                  </TRADESTRD>
                  <CUSOFFPREOFFRES>
                    <RefNumRES1>{arrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
                  </CUSOFFPREOFFRES>
                  {buildEnRouteEvent(arrivalNotificationRequest.enRouteEvents, Header.Constants.languageCode)}
                </CC007A>
              )
            }

            trim(convertToXml.buildXml(arrivalNotificationRequest)(genDateTime).right.toOption.value) mustBe trim(validXml)
          }
      }
    }

    "must return correct nodes for a minimal arrival notification request" in {

      val result = trim(convertToXml.buildXml(minimalArrivalNotificationRequest)(dateTime).right.toOption.value)
      result mustBe minimalValidXml
    }

    "must add transit wrapper to an existing xml" in {

      val result = trim(convertToXml.buildXmlWithTransitWrapper(minimalValidXml).right.toOption.value)
      val expectedResult =
        <transitRequest xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                        xsi:noNamespaceSchemaLocation="../../schema/request/request.xsd">{minimalValidXml}</transitRequest>

      result mustBe expectedResult
    }

    "must return correct nodes for a minimal arrival notification request with an EnRouteEvent" in {

      val arrivalNotificationRequestWithIncident: ArrivalNotificationRequest =
        ArrivalNotificationRequest(
          meta = Meta(
            messageSender = MessageSender("LOCAL", "EORI&123"),
            interchangeControlReference = InterchangeControlReference("2019", 1)
          ),
          header = Header("MovementReferenceNumber", None, "arrivalNotificationPlace", None, NormalProcedureFlag),
          traderDestination = TraderDestination(None, None, None, None, None, None),
          customsOfficeOfPresentation = CustomsOfficeOfPresentation("PresentationOffice"),
          enRouteEvents = Some(
            Seq(
              EnRouteEvent(
                place = "place",
                countryCode = "GB",
                alreadyInNcts = true,
                eventDetails = Incident(None, Endorsement(None, None, None, None)),
                seals = None
              )
            ))
        )

      val minimalValidXmlWithEnrouteEvent: Node =
        <CC007A>
          <SynIdeMES1>{arrivalNotificationRequestWithIncident.syntaxIdentifier}</SynIdeMES1>
          <SynVerNumMES2>{arrivalNotificationRequestWithIncident.meta.syntaxVersionNumber}</SynVerNumMES2>
          <MesSenMES3>{arrivalNotificationRequestWithIncident.meta.messageSender.toString}</MesSenMES3>
          <MesRecMES6>{arrivalNotificationRequestWithIncident.meta.messageRecipient}</MesRecMES6>
          <DatOfPreMES9>{dateOfPreparation}</DatOfPreMES9>
          <TimOfPreMES10>{timeOfPreparation}</TimOfPreMES10>
          <IntConRefMES11>{arrivalNotificationRequestWithIncident.meta.interchangeControlReference.toString}</IntConRefMES11>
          <AppRefMES14>{arrivalNotificationRequestWithIncident.meta.applicationReference}</AppRefMES14>
          <TesIndMES18>{arrivalNotificationRequestWithIncident.meta.testIndicator}</TesIndMES18>
          <MesIdeMES19>{arrivalNotificationRequestWithIncident.meta.messageIndication}</MesIdeMES19>
          <MesTypMES20>{arrivalNotificationRequestWithIncident.messageCode.code}</MesTypMES20>
          <HEAHEA>
            <DocNumHEA5>{arrivalNotificationRequestWithIncident.header.movementReferenceNumber}</DocNumHEA5>
            <ArrNotPlaHEA60>{arrivalNotificationRequestWithIncident.header.arrivalNotificationPlace}</ArrNotPlaHEA60>
            <ArrNotPlaHEA60LNG>{Header.Constants.languageCode.code}</ArrNotPlaHEA60LNG>
            <ArrAgrLocOfGooHEA63LNG>{Header.Constants.languageCode.code}</ArrAgrLocOfGooHEA63LNG>
            <SimProFlaHEA132>{arrivalNotificationRequestWithIncident.header.procedureTypeFlag.code}</SimProFlaHEA132>
            <ArrNotDatHEA141>{dateOfPreparation}</ArrNotDatHEA141>
          </HEAHEA>
          <TRADESTRD>
            <NADLNGRD>{Header.Constants.languageCode.code}</NADLNGRD>
          </TRADESTRD>
          <CUSOFFPREOFFRES>
            <RefNumRES1>{arrivalNotificationRequestWithIncident.customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
          </CUSOFFPREOFFRES>
          {
            arrivalNotificationRequestWithIncident.enRouteEvents.value.map {
              enrouteEvent =>
                <ENROUEVETEV>
                  <PlaTEV10>{enrouteEvent.place}</PlaTEV10>
                  <PlaTEV10LNG>{Header.Constants.languageCode.code}</PlaTEV10LNG>
                  <CouTEV13>{enrouteEvent.countryCode}</CouTEV13>
                  <CTLCTL>
                    <AlrInNCTCTL29>1</AlrInNCTCTL29>
                  </CTLCTL>
                  <INCINC>
                    <IncFlaINC3>1</IncFlaINC3>
                    <IncInfINC4LNG>{Header.Constants.languageCode.code}</IncInfINC4LNG>
                    <EndAutINC7LNG>{Header.Constants.languageCode.code}</EndAutINC7LNG>
                    <EndPlaINC10LNG>{Header.Constants.languageCode.code}</EndPlaINC10LNG>
                  </INCINC>
                </ENROUEVETEV>
            }
          }
        </CC007A>

      val result = trim(convertToXml.buildXml(arrivalNotificationRequestWithIncident)(dateTime).right.toOption.value)

      result mustBe trim(minimalValidXmlWithEnrouteEvent)
    }
  }

  private def buildSeals(seals: Seq[Seal]): NodeSeq = {
    val sealsXml = seals.map {
      seal =>
        <SEAIDSI1>
              <SeaIdeSI11>
                {seal.numberOrMark}
              </SeaIdeSI11>
              <SeaIdeSI11LNG>EN</SeaIdeSI11LNG>
            </SEAIDSI1>
    }

    <SEAINFSF1>
          <SeaNumSF12>
            {seals.size}
          </SeaNumSF12>{sealsXml}
        </SEAINFSF1>
  }

  private def buildEnRouteEvent(enRouteEvents: Option[Seq[EnRouteEvent]], languageCode: LanguageCode): NodeSeq = enRouteEvents match {
    case Some(events) =>
      events.map {
        event =>
          <ENROUEVETEV> {
            buildAndEncodeElem(event.place,"PlaTEV10") ++
              buildAndEncodeElem(languageCode,"PlaTEV10LNG") ++
              buildAndEncodeElem(event.countryCode,"CouTEV13")
            }
            <CTLCTL> {
              buildAndEncodeElem(event.alreadyInNcts,"AlrInNCTCTL29")
              }
            </CTLCTL> {
            buildIncidentType(event.eventDetails, event.seals, languageCode)
            }
          </ENROUEVETEV>
      }
    case None => NodeSeq.Empty
  }

  private def buildIncidentType(event: EventDetails, sealsOpt: Option[Seq[Seal]], languageCode: LanguageCode): NodeSeq = {
    val seals = sealsOpt.fold(NodeSeq.Empty) {
      seal =>
        buildSeals(seal)
    }
    event match {
      case incident: Incident =>
        <INCINC>
        {
        buildIncidentFlag(incident.information.isDefined) ++
          buildOptionalElem(incident.information, "IncInfINC4") ++
          buildAndEncodeElem(languageCode, "IncInfINC4LNG") ++
          buildOptionalElem(incident.endorsement.date, "EndDatINC6") ++
          buildOptionalElem(incident.endorsement.authority, "EndAutINC7") ++
          buildAndEncodeElem(languageCode, "EndAutINC7LNG") ++
          buildOptionalElem(incident.endorsement.place, "EndPlaINC10") ++
          buildAndEncodeElem(languageCode, "EndPlaINC10LNG") ++
          buildOptionalElem(incident.endorsement.country, "EndCouINC12")
        }
      </INCINC> ++ seals

      case containerTranshipment: ContainerTranshipment =>
        seals ++
          <TRASHP> {
        buildOptionalElem(containerTranshipment.endorsement.date, "EndDatSHP60") ++
        buildOptionalElem(containerTranshipment.endorsement.authority, "EndAutSHP61") ++
        buildAndEncodeElem(languageCode,"EndAutSHP61LNG") ++
        buildOptionalElem(containerTranshipment.endorsement.place, "EndPlaSHP63") ++
        buildAndEncodeElem(languageCode,"EndPlaSHP63LNG") ++
        buildOptionalElem(containerTranshipment.endorsement.country, "EndCouSHP65") ++
        containerTranshipment.containers.map {
          container =>
            <CONNR3>
              {buildAndEncodeElem(container.containerNumber, "ConNumNR31")}
            </CONNR3>
          }
        }
      </TRASHP>

      case vehicularTranshipment: VehicularTranshipment =>
        seals ++
          <TRASHP> {
        buildAndEncodeElem(vehicularTranshipment.transportIdentity,"NewTraMeaIdeSHP26") ++
        buildAndEncodeElem(languageCode,"NewTraMeaIdeSHP26LNG") ++
        buildAndEncodeElem(vehicularTranshipment.transportCountry,"NewTraMeaNatSHP54") ++
        buildOptionalElem(vehicularTranshipment.endorsement.date, "EndDatSHP60") ++
        buildOptionalElem(vehicularTranshipment.endorsement.authority, "EndAutSHP61") ++
        buildAndEncodeElem(languageCode, "EndAutSHP61LNG") ++
        buildOptionalElem(vehicularTranshipment.endorsement.place, "EndPlaSHP63") ++
        buildAndEncodeElem(languageCode, "EndPlaSHP63LNG") ++
        buildOptionalElem(vehicularTranshipment.endorsement.country, "EndCouSHP65") ++
        {
          vehicularTranshipment.containers match {
            case Some(containers) =>
              containers.map {
                container =>
                  <CONNR3>
                    {buildAndEncodeElem(container.containerNumber, "ConNumNR31")}
                  </CONNR3>
              }
            case _ => NodeSeq.Empty
            }
          }
        }
      </TRASHP>

    }
  }

  private def hasEoriWithNormalProcedure()(implicit arrivalNotificationRequest: ArrivalNotificationRequest): Boolean =
    arrivalNotificationRequest.traderDestination.eori.isDefined &&
      arrivalNotificationRequest.header.procedureTypeFlag.equals(NormalProcedureFlag)

  private def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
    case Some(result: String) => {
      val encodeResult = StringEscapeUtils.escapeXml11(result)
      loadString(s"<$elementTag>$encodeResult</$elementTag>")
    }
    case Some(result: LocalDate)     => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case Some(result: LocalDateTime) => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case _                           => NodeSeq.Empty
  }

  private def buildAndEncodeElem[A](value: A, elementTag: String): NodeSeq = value match {
    case result: String => {
      val encodeResult = StringEscapeUtils.escapeXml11(result)
      loadString(s"<$elementTag>$encodeResult</$elementTag>")
    }
    case result: Boolean           => loadString(s"<$elementTag>${if (result) 1 else 0}</$elementTag>")
    case result: LocalDate         => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case result: LocalDateTime     => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case result: LanguageCode      => loadString(s"<$elementTag>${result.code}</$elementTag>")
    case result: ProcedureTypeFlag => loadString(s"<$elementTag>${result.code}</$elementTag>")
    case _                         => NodeSeq.Empty
  }

  private def buildIncidentFlag(hasIncidentInformation: Boolean): NodeSeq =
    if (hasIncidentInformation) NodeSeq.Empty else <IncFlaINC3>1</IncFlaINC3>

}
