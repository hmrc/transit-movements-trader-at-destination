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

import scala.xml.Utility.trim
import scala.xml.XML.loadString

class EnRouteEventSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "EnRouteEvent" - {

    "must create valid xml with Incident and seal" in {

      forAll(arbitrary[EnRouteEvent], arbitrary[Seal], arbitrary[Incident]) {
        (enRouteEvent, seal, incident) =>
          val enRouteEventWithSealAndIncident = enRouteEvent.copy(seals = Some(Seq(seal)), eventDetails = Some(incident))

          val result = {
            <ENROUEVETEV>
            <PlaTEV10>{enRouteEventWithSealAndIncident.place}</PlaTEV10>
            <PlaTEV10LNG>{LanguageCodeEnglish.code}</PlaTEV10LNG>
            <CouTEV13>{enRouteEventWithSealAndIncident.countryCode}</CouTEV13>
            <CTLCTL>
              <AlrInNCTCTL29>{if (enRouteEventWithSealAndIncident.alreadyInNcts) 1 else 0}</AlrInNCTCTL29>
            </CTLCTL>
            {
              incident.toXml
            }
           <SEAINFSF1>
             <SeaNumSF12>1</SeaNumSF12>
             {
              seal.toXml
             }
           </SEAINFSF1>
          </ENROUEVETEV>
          }

          trim(enRouteEventWithSealAndIncident.toXml) mustBe trim(loadString(result.toString))
      }
    }
    "must create valid xml with container transhipment and seal" in {

      forAll(arbitrary[EnRouteEvent], arbitrary[Seal], arbitrary[ContainerTranshipment]) {
        (enRouteEvent, seal, containerTranshipment) =>
          val enRouteEventWithContainer = enRouteEvent.copy(seals = Some(Seq(seal)), eventDetails = Some(containerTranshipment))

          val result = {
            <ENROUEVETEV>
              <PlaTEV10>{enRouteEventWithContainer.place}</PlaTEV10>
              <PlaTEV10LNG>{LanguageCodeEnglish.code}</PlaTEV10LNG>
              <CouTEV13>{enRouteEventWithContainer.countryCode}</CouTEV13>
              <CTLCTL>
                <AlrInNCTCTL29>{if (enRouteEventWithContainer.alreadyInNcts) 1 else 0}</AlrInNCTCTL29>
              </CTLCTL>
              <SEAINFSF1>
                <SeaNumSF12>1</SeaNumSF12>
                {
                seal.toXml
                }
              </SEAINFSF1>
              {
              containerTranshipment.toXml
              }
            </ENROUEVETEV>
          }

          trim(enRouteEventWithContainer.toXml) mustBe trim(loadString(result.toString))
      }
    }

    "must create valid xml with vehicular transhipment with seal" in {

      forAll(arbitrary[EnRouteEvent], arbitrary[Seal], arbitrary[VehicularTranshipment]) {
        (enRouteEvent, seal, vehicularTranshipment) =>
          val enRouteEventWithVehicle = enRouteEvent.copy(seals = Some(Seq(seal)), eventDetails = Some(vehicularTranshipment))

          val result = {
            <ENROUEVETEV>
              <PlaTEV10>{enRouteEventWithVehicle.place}</PlaTEV10>
              <PlaTEV10LNG>{LanguageCodeEnglish.code}</PlaTEV10LNG>
              <CouTEV13>{enRouteEventWithVehicle.countryCode}</CouTEV13>
              <CTLCTL>
                <AlrInNCTCTL29>{if (enRouteEventWithVehicle.alreadyInNcts) 1 else 0}</AlrInNCTCTL29>
              </CTLCTL>
              <SEAINFSF1>
                <SeaNumSF12>1</SeaNumSF12>
                {
                seal.toXml
                }
              </SEAINFSF1>
              {
              vehicularTranshipment.toXml
              }
            </ENROUEVETEV>
          }

          trim(enRouteEventWithVehicle.toXml) mustBe trim(loadString(result.toString))
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
          val json = Json.toJson(enrouteEvent)

          json.validate[EnRouteEvent] mustEqual JsSuccess(enrouteEvent)
      }
    }
  }

  def buildEnrouteEvent(enrouteEvent: EnRouteEvent): JsObject =
    Json.obj(
      "place"         -> enrouteEvent.place,
      "countryCode"   -> enrouteEvent.countryCode,
      "alreadyInNcts" -> enrouteEvent.alreadyInNcts
    ) ++ {
      enrouteEvent.eventDetails.fold(JsObject.empty)(ed => Json.obj("eventDetails" -> Json.toJson(ed)))
    } ++ {
      enrouteEvent.seals match {
        case Some(seals) =>
          Json.obj("seals" -> Json.toJson(seals))
        case _ =>
          JsObject.empty
      }
    }

}
