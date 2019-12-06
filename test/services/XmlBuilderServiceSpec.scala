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

import generators.MessageGenerators
import models.EnRouteEvent
import models.EventDetails
import models.Incident
import models.Transhipment
import models.messages.request.ArrivalNotificationRequest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import utils.Format

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

  import XmlBuilderServiceSpec._

  private val convertToXml = new XmlBuilderService()

  "XmlBuilderService" - {
    "must return correct nodes" in {
      val localDateTime: Gen[LocalDateTime] = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)

      forAll(arbitrary[ArrivalNotificationRequest](arbitraryArrivalNotificationRequest), localDateTime) {
        (arrivalNotificationRequest, dateTime) =>
          whenever(hasEoriWithNormalProcedure()(arrivalNotificationRequest)) {
            val dateOfPreparation = Format.dateFormatted(dateTime)
            val timeOfPreparation = Format.timeFormatted(dateTime)

            val validXml: Node = {
              trim(<CC007A>
              <SynIdeMES1>
                {arrivalNotificationRequest.syntaxIdentifier}
              </SynIdeMES1> <SynVerNumMES2>
                {arrivalNotificationRequest.meta.syntaxVersionNumber}
              </SynVerNumMES2> <MesSenMES3>
                {arrivalNotificationRequest.meta.messageSender.toString}
              </MesSenMES3> <MesRecMES6>
                {arrivalNotificationRequest.meta.messageRecipient}
              </MesRecMES6> <DatOfPreMES9>
                {dateOfPreparation}
              </DatOfPreMES9> <TimOfPreMES10>
                {timeOfPreparation}
              </TimOfPreMES10> <IntConRefMES11>
                {arrivalNotificationRequest.meta.interchangeControlReference.toString}
              </IntConRefMES11> <AppRefMES14>
                {arrivalNotificationRequest.meta.applicationReference}
              </AppRefMES14> <TesIndMES18>
                {arrivalNotificationRequest.meta.testIndicator}
              </TesIndMES18> <MesIdeMES19>
                {arrivalNotificationRequest.meta.messageIndication}
              </MesIdeMES19> <MesTypMES20>
                {arrivalNotificationRequest.messageCode.code}
              </MesTypMES20> <HEAHEA>
                <DocNumHEA5>
                  {arrivalNotificationRequest.header.movementReferenceNumber}
                </DocNumHEA5>{buildOptionalElem(arrivalNotificationRequest.header.customsSubPlace, "CusSubPlaHEA66")}<ArrNotPlaHEA60>
                  {arrivalNotificationRequest.header.arrivalNotificationPlace}
                </ArrNotPlaHEA60> <ArrNotPlaHEA60LNG>
                  {arrivalNotificationRequest.header.languageCode}
                </ArrNotPlaHEA60LNG> <ArrAgrLocOfGooHEA63LNG>
                  {arrivalNotificationRequest.header.languageCode}
                </ArrAgrLocOfGooHEA63LNG>{buildOptionalElem(arrivalNotificationRequest.header.arrivalAgreedLocationOfGoods, "ArrAutLocOfGooHEA65")}<SimProFlaHEA132>
                  {arrivalNotificationRequest.header.simplifiedProcedureFlag}
                </SimProFlaHEA132> <ArrNotDatHEA141>
                  {dateOfPreparation}
                </ArrNotDatHEA141>
              </HEAHEA> <TRADESTRD>
                {buildOptionalElem(arrivalNotificationRequest.traderDestination.name, "NamTRD7")}{buildOptionalElem(arrivalNotificationRequest.traderDestination.streetAndNumber, "StrAndNumTRD22")}{buildOptionalElem(arrivalNotificationRequest.traderDestination.postCode, "PosCodTRD23")}{buildOptionalElem(arrivalNotificationRequest.traderDestination.city, "CitTRD24")}{buildOptionalElem(arrivalNotificationRequest.traderDestination.countryCode, "CouTRD25")}<NADLNGRD>
                  {arrivalNotificationRequest.header.languageCode}
                </NADLNGRD>{buildOptionalElem(arrivalNotificationRequest.traderDestination.eori, "TINTRD59")}
              </TRADESTRD> <CUSOFFPREOFFRES>
                <RefNumRES1>
                  {arrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice}
                </RefNumRES1>
              </CUSOFFPREOFFRES>{buildEnRouteEvent(arrivalNotificationRequest.enRouteEvents, arrivalNotificationRequest.header.languageCode)}
            </CC007A>)
            }

            trim(convertToXml.buildXml(arrivalNotificationRequest)(dateTime).right.toOption.value) mustBe validXml
          }
      }
    }
  }

  private def buildEnRouteEvent(enRouteEvents: Option[Seq[EnRouteEvent]], languageCode: String): NodeSeq = enRouteEvents match {
    case None => NodeSeq.Empty
    case Some(events) =>
      events.map {
        event =>
          <ENROUEVETEV>
        <PlaTEV10>
          {event.place}
        </PlaTEV10> <PlaTEV10LNG>
        {languageCode}
      </PlaTEV10LNG> <CouTEV13>
        {event.countryCode}
      </CouTEV13> <CTLCTL>
        <AlrInNCTCTL29>
          {if (event.alreadyInNcts) 1 else 0}
        </AlrInNCTCTL29>
      </CTLCTL>{buildIncidentType(event.eventDetails, languageCode)}
      </ENROUEVETEV>
      }
  }

  private def buildIncidentType(event: EventDetails, languageCode: String): NodeSeq = event match {
    case incident: Incident => {
      <INCINC>
        {buildIncidentFlag(incident.information.isDefined)}{buildOptionalElem(incident.information, "IncInfINC4")}<IncInfINC4LNG>
        {languageCode}
      </IncInfINC4LNG>{buildOptionalElem(incident.endorsement.date, "EndDatINC6")}{buildOptionalElem(incident.endorsement.authority, "EndAutINC7")}<EndAutINC7LNG>
        {languageCode}
      </EndAutINC7LNG>{buildOptionalElem(incident.endorsement.place, "EndPlaINC10")}<EndPlaINC10LNG>
        {languageCode}
      </EndPlaINC10LNG>{buildOptionalElem(incident.endorsement.country, "EndCouINC12")}
      </INCINC>
    }
    case _: Transhipment => NodeSeq.Empty //TODO: implement transhipment
  }
}

object XmlBuilderServiceSpec {
  private def hasEoriWithNormalProcedure()(implicit arrivalNotificationRequest: ArrivalNotificationRequest): Boolean =
    arrivalNotificationRequest.traderDestination.eori.isDefined && arrivalNotificationRequest.header.simplifiedProcedureFlag.equals("0")

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
