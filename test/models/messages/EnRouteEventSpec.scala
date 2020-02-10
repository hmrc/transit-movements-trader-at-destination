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

package models.messages

import generators.ModelGenerators
import models.behaviours.JsonBehaviours
import models.request.LanguageCodeEnglish
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import utils.Format

import scala.xml.NodeSeq
import scala.xml.Utility.trim
import scala.xml.XML.loadString

class EnRouteEventSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "EnRouteEvent" - {

    "must create valid xml with Incident and seals" in {

      forAll(arbitrary[EnRouteEvent], arbitrary[Seal], arbitrary[Incident]) {
        (enRouteEvent, seal, incident) =>
          val enRouteEventWithSealAndIncident = enRouteEvent.copy(seals = Some(Seq(seal)), eventDetails = incident)

          val incidentInformationOrFlagNode = incident.information
            .map {
              information =>
                <IncInfINC4>{information}</IncInfINC4>
            }
            .getOrElse {
              <IncFlaINC3>1</IncFlaINC3>
            }
          val endorsementDateNode = incident.endorsement.date.map {
            date =>
              <EndDatINC6>{Format.dateFormatted(date)}</EndDatINC6>
          }
          val endorsementAuthority = incident.endorsement.authority.map {
            authority =>
              <EndAutINC7>{authority}</EndAutINC7>
          }
          val endorsementPlace = incident.endorsement.place.map {
            place =>
              <EndPlaINC10>{place}</EndPlaINC10>
          }
          val endorsementCountry = incident.endorsement.country.map {
            country =>
              <EndCouINC12>{country}</EndCouINC12>
          }

          val result = {
            <ENROUEVETEV>
            <PlaTEV10>{enRouteEventWithSealAndIncident.place}</PlaTEV10>
            <PlaTEV10LNG>{LanguageCodeEnglish.code}</PlaTEV10LNG>
            <CouTEV13>{enRouteEventWithSealAndIncident.countryCode}</CouTEV13>
            <CTLCTL>
              <AlrInNCTCTL29>{if (enRouteEventWithSealAndIncident.alreadyInNcts) 1 else 0}</AlrInNCTCTL29>
            </CTLCTL>
           <INCINC>
             {
              incidentInformationOrFlagNode
             }
             <IncInfINC4LNG>{LanguageCodeEnglish.code}</IncInfINC4LNG>
             {
              endorsementDateNode.getOrElse(NodeSeq.Empty) ++
               endorsementAuthority.getOrElse(NodeSeq.Empty)
             }
             <EndAutINC7LNG>{LanguageCodeEnglish.code}</EndAutINC7LNG>
             {
              endorsementPlace.getOrElse(NodeSeq.Empty)
             }
             <EndPlaINC10LNG>{LanguageCodeEnglish.code}</EndPlaINC10LNG>
             {
              endorsementCountry.getOrElse(NodeSeq.Empty)
             }
           </INCINC>
           <SEAINFSF1>
             <SeaNumSF12>1</SeaNumSF12>
             <SEAIDSI1>
               <SeaIdeSI11>{seal.numberOrMark}</SeaIdeSI11>
               <SeaIdeSI11LNG>{LanguageCodeEnglish.code}</SeaIdeSI11LNG>
             </SEAIDSI1>
           </SEAINFSF1>
          </ENROUEVETEV>
          }

          trim(enRouteEventWithSealAndIncident.toXml) mustBe trim(loadString(result.toString))
      }
    }

    "must serialise" in {
      forAll(arbitrary[EnRouteEvent]) {
        enrouteEvent =>
          val json = buildEnrouteEvent(enrouteEvent)

          Json.toJson(enrouteEvent) mustEqual json
      }
    }

    "must deserialise" in {
      forAll(arbitrary[EnRouteEvent]) {
        enrouteEvent =>
          val json = buildEnrouteEvent(enrouteEvent)

          json.validate[EnRouteEvent] mustEqual JsSuccess(enrouteEvent)
      }
    }
  }

  def buildEnrouteEvent(enrouteEvent: EnRouteEvent): JsObject =
    Json.obj(
      "place"         -> enrouteEvent.place,
      "countryCode"   -> enrouteEvent.countryCode,
      "alreadyInNcts" -> enrouteEvent.alreadyInNcts,
      "eventDetails"  -> Json.toJson(enrouteEvent.eventDetails)
    ) ++ {
      enrouteEvent.seals match {
        case Some(seals) =>
          Json.obj("seals" -> Json.toJson(seals))
        case _ =>
          JsObject.empty
      }
    }
}
