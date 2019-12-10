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

import generators.MessageGenerators
import models.EnRouteEvent
import models.Endorsement
import models.EventDetails
import models.Incident
import models.messages.request._
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import utils.Format

import scala.xml.Node
import scala.xml.Utility.trim

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
        MessageSender("LOCAL", "EORI123"),
        InterchangeControlReference("2019", 1)
      ),
      header = Header("MovementReferenceNumber", None, "arrivalNotificationPlace", None, "SimplifiedProcedureFlag"),
      traderDestination = TraderDestination(None, None, None, None, None, None),
      customsOfficeOfPresentation = CustomsOfficeOfPresentation("PresentationOffice"),
      enRouteEvents = None
    )

  private val arrivalNotificationRequestWithIncident: ArrivalNotificationRequest =
    ArrivalNotificationRequest(
      meta = Meta(
        messageSender = MessageSender("LOCAL", "EORI123"),
        interchangeControlReference = InterchangeControlReference("2019", 1)
      ),
      header = Header("MovementReferenceNumber", None, "arrivalNotificationPlace", None, "SimplifiedProcedureFlag"),
      traderDestination = TraderDestination(None, None, None, None, None, None),
      customsOfficeOfPresentation = CustomsOfficeOfPresentation("PresentationOffice"),
      enRouteEvents = Some(
        Seq(
          EnRouteEvent(
            place = "place",
            countryCode = "GB",
            alreadyInNcts = true,
            eventDetails = Incident(None, Endorsement(None, None, None, None)),
            seals = Some(Seq("seal1", "seal2"))
          )
        ))
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
            <ArrNotPlaHEA60LNG>{minimalArrivalNotificationRequest.header.languageCode}</ArrNotPlaHEA60LNG>
            <ArrAgrLocOfGooHEA63LNG>{minimalArrivalNotificationRequest.header.languageCode}</ArrAgrLocOfGooHEA63LNG>
            <SimProFlaHEA132>{minimalArrivalNotificationRequest.header.simplifiedProcedureFlag}</SimProFlaHEA132>
            <ArrNotDatHEA141>{dateOfPreparation}</ArrNotDatHEA141>
          </HEAHEA>
          <TRADESTRD>
            <NADLNGRD>{minimalArrivalNotificationRequest.header.languageCode}</NADLNGRD>
          </TRADESTRD>
          <CUSOFFPREOFFRES>
            <RefNumRES1>{minimalArrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
          </CUSOFFPREOFFRES>
        </CC007A>
    )
  }

  private def minimalValidXmlWithIncident: Node =
    trim(
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
          <ArrNotPlaHEA60LNG>{arrivalNotificationRequestWithIncident.header.languageCode}</ArrNotPlaHEA60LNG>
          <ArrAgrLocOfGooHEA63LNG>{arrivalNotificationRequestWithIncident.header.languageCode}</ArrAgrLocOfGooHEA63LNG>
          <SimProFlaHEA132>{arrivalNotificationRequestWithIncident.header.simplifiedProcedureFlag}</SimProFlaHEA132>
          <ArrNotDatHEA141>{dateOfPreparation}</ArrNotDatHEA141>
        </HEAHEA>
        <TRADESTRD>
          <NADLNGRD>{arrivalNotificationRequestWithIncident.header.languageCode}</NADLNGRD>
        </TRADESTRD>
        <CUSOFFPREOFFRES>
          <RefNumRES1>{arrivalNotificationRequestWithIncident.customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
        </CUSOFFPREOFFRES>
        {
          arrivalNotificationRequestWithIncident.enRouteEvents.get.map {
            enrouteEvent =>
              <ENROUEVETEV>
                <PlaTEV10>{enrouteEvent.place}</PlaTEV10>
                <PlaTEV10LNG>{arrivalNotificationRequestWithIncident.header.languageCode}</PlaTEV10LNG>
                <CouTEV13>{enrouteEvent.countryCode}</CouTEV13>
                <CTLCTL>
                  <AlrInNCTCTL29>{if (enrouteEvent.alreadyInNcts) 1 else 0}</AlrInNCTCTL29>
                </CTLCTL>
                <INCINC>
                  <IncInfINC4LNG>{arrivalNotificationRequestWithIncident.header.languageCode}</IncInfINC4LNG>
                  <EndAutINC7LNG>{arrivalNotificationRequestWithIncident.header.languageCode}</EndAutINC7LNG>
                  <EndPlaINC10LNG>{arrivalNotificationRequestWithIncident.header.languageCode}</EndPlaINC10LNG>
                </INCINC>
              </ENROUEVETEV>
          }
        }

      </CC007A>
    )

  "XmlBuilderService" - {
    "must return correct nodes" in {
      ???
    }

    "must return correct nodes for a minimal arrival notification request" in {
      trim(convertToXml.buildXml(minimalArrivalNotificationRequest)(dateTime).right.toOption.value) mustBe minimalValidXml
    }

    "must return correct nodes for a minimal arrival notification request with an incident" in {
      trim(convertToXml.buildXml(arrivalNotificationRequestWithIncident)(dateTime).right.toOption.value) mustBe minimalValidXmlWithIncident
    }
  }
}
