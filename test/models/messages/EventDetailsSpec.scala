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
import org.scalatest.OptionValues._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import utils.Format

import scala.xml.NodeSeq
import scala.xml.Utility.trim
import scala.xml.XML.loadString

class EventDetailsSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "Incident" - {

    "must create valid xml" in {

      forAll(arbitrary[Incident]) {
        incident =>
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

          val expectedResult =
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

          trim(incident.toXml) mustEqual trim(loadString(expectedResult.toString))
      }
    }

    "must deserialise" in {

      forAll(arbitrary[Incident]) {
        incident =>
          val json = incidentJson(incident)
          json.validate[Incident] mustEqual JsSuccess(incident)
      }
    }

    "must serialise" in {

      forAll(arbitrary[Incident]) {
        incident =>
          val json = incidentJson(incident)
          Json.toJson(incident) mustEqual json
      }
    }
  }

  "Container transhipment" - {

    "must create valid xml" in {

      forAll(arbitrary[ContainerTranshipment], arbitrary[Container], arbitrary[Container]) {
        (transhipment, container1, container2) =>
          val containerTranshipment: ContainerTranshipment = {
            transhipment.copy(containers = Seq(container1, container2))
          }

          val endorsementDateNode = containerTranshipment.endorsement.date.map {
            date =>
              <EndDatSHP60>{Format.dateFormatted(date)}</EndDatSHP60>
          }
          val endorsementAuthority = containerTranshipment.endorsement.authority.map {
            authority =>
              <EndAutSHP61>{authority}</EndAutSHP61>
          }
          val endorsementPlace = containerTranshipment.endorsement.place.map {
            place =>
              <EndPlaSHP63>{place}</EndPlaSHP63>
          }
          val endorsementCountry = containerTranshipment.endorsement.country.map {
            country =>
              <EndCouSHP65>{country}</EndCouSHP65>
          }

          val expectedResult =
            <TRASHP>
              {
                endorsementDateNode.getOrElse(NodeSeq.Empty) ++
                endorsementAuthority.getOrElse(NodeSeq.Empty)
              }
              <EndAutSHP61LNG>{LanguageCodeEnglish.code}</EndAutSHP61LNG>
              {
                endorsementPlace.getOrElse(NodeSeq.Empty)
              }
              <EndPlaSHP63LNG>{LanguageCodeEnglish.code}</EndPlaSHP63LNG>
              {
                endorsementCountry.getOrElse(NodeSeq.Empty)
              }
              <CONNR3>
                <ConNumNR31>{containerTranshipment.containers.head.containerNumber}</ConNumNR31>
              </CONNR3>
              <CONNR3>
                <ConNumNR31>{containerTranshipment.containers(1).containerNumber}</ConNumNR31>
              </CONNR3>
            </TRASHP>

          trim(containerTranshipment.toXml) mustEqual trim(loadString(expectedResult.toString))
      }
    }

    "must fail to construct when given an empty sequence of containers" in {

      forAll(arbitrary[Endorsement]) {
        endorsement =>
          intercept[IllegalArgumentException] {
            ContainerTranshipment(endorsement, Seq.empty)
          }
      }
    }

    "must deserialise" in {

      forAll(arbitrary[ContainerTranshipment]) {
        containerTranshipment =>
          val json = containerTranshipmentJson(containerTranshipment)
          json.validate[ContainerTranshipment] mustEqual JsSuccess(containerTranshipment)
      }
    }

    "must serialise" in {

      forAll(arbitrary[ContainerTranshipment]) {
        containerTranshipment =>
          val json = containerTranshipmentJson(containerTranshipment)
          Json.toJson(containerTranshipment) mustEqual json
      }
    }
  }

  "Vehicular transhipment" - {

    "must create valid xml without containers" in {
      forAll(arbitrary[VehicularTranshipment]) {
        transhipment =>
          val vehicularTranshipment = transhipment.copy(containers = None)

          val endorsementDateNode = vehicularTranshipment.endorsement.date.map {
            date =>
              <EndDatSHP60>{Format.dateFormatted(date)}</EndDatSHP60>
          }
          val endorsementAuthority = vehicularTranshipment.endorsement.authority.map {
            authority =>
              <EndAutSHP61>{authority}</EndAutSHP61>
          }
          val endorsementPlace = vehicularTranshipment.endorsement.place.map {
            place =>
              <EndPlaSHP63>{place}</EndPlaSHP63>
          }
          val endorsementCountry = vehicularTranshipment.endorsement.country.map {
            country =>
              <EndCouSHP65>{country}</EndCouSHP65>
          }

          val expectedResult =
            <TRASHP>
              <NewTraMeaIdeSHP26>{vehicularTranshipment.transportIdentity}</NewTraMeaIdeSHP26>
              <NewTraMeaIdeSHP26LNG>{LanguageCodeEnglish.code}</NewTraMeaIdeSHP26LNG>
              <NewTraMeaNatSHP54>{vehicularTranshipment.transportCountry}</NewTraMeaNatSHP54>
              {
                endorsementDateNode.getOrElse(NodeSeq.Empty) ++
                endorsementAuthority.getOrElse(NodeSeq.Empty)
              }
              <EndAutSHP61LNG>{LanguageCodeEnglish.code}</EndAutSHP61LNG>
              {
                endorsementPlace.getOrElse(NodeSeq.Empty)
              }
              <EndPlaSHP63LNG>{LanguageCodeEnglish.code}</EndPlaSHP63LNG>
              {
                endorsementCountry.getOrElse(NodeSeq.Empty)
              }
            </TRASHP>

          trim(vehicularTranshipment.toXml) mustEqual trim(loadString(expectedResult.toString))
      }
    }

    "must create valid xml with containers" in {
      forAll(arbitrary[VehicularTranshipment], arbitrary[Container], arbitrary[Container]) {
        (transhipment, container1, container2) =>
          val vehicularTranshipment = transhipment.copy(containers = Some(Seq(container1, container2)))

          val endorsementDateNode = vehicularTranshipment.endorsement.date.map {
            date =>
              <EndDatSHP60>{Format.dateFormatted(date)}</EndDatSHP60>
          }
          val endorsementAuthority = vehicularTranshipment.endorsement.authority.map {
            authority =>
              <EndAutSHP61>{authority}</EndAutSHP61>
          }
          val endorsementPlace = vehicularTranshipment.endorsement.place.map {
            place =>
              <EndPlaSHP63>{place}</EndPlaSHP63>
          }
          val endorsementCountry = vehicularTranshipment.endorsement.country.map {
            country =>
              <EndCouSHP65>{country}</EndCouSHP65>
          }

          val expectedResult =
            <TRASHP>
              <NewTraMeaIdeSHP26>{vehicularTranshipment.transportIdentity}</NewTraMeaIdeSHP26>
              <NewTraMeaIdeSHP26LNG>{LanguageCodeEnglish.code}</NewTraMeaIdeSHP26LNG>
              <NewTraMeaNatSHP54>{vehicularTranshipment.transportCountry}</NewTraMeaNatSHP54>
              {
              endorsementDateNode.getOrElse(NodeSeq.Empty) ++
                endorsementAuthority.getOrElse(NodeSeq.Empty)
              }
              <EndAutSHP61LNG>{LanguageCodeEnglish.code}</EndAutSHP61LNG>
              {
              endorsementPlace.getOrElse(NodeSeq.Empty)
              }
              <EndPlaSHP63LNG>{LanguageCodeEnglish.code}</EndPlaSHP63LNG>
              {
              endorsementCountry.getOrElse(NodeSeq.Empty)
              }
              <CONNR3>
                <ConNumNR31>{vehicularTranshipment.containers.value.head.containerNumber}</ConNumNR31>
              </CONNR3>
              <CONNR3>
                <ConNumNR31>{vehicularTranshipment.containers.value(1).containerNumber}</ConNumNR31>
              </CONNR3>
            </TRASHP>

          trim(vehicularTranshipment.toXml) mustEqual trim(loadString(expectedResult.toString))
      }
    }

    "must deserialise" in {

      forAll(arbitrary[VehicularTranshipment]) {
        vehicularTranshipment =>
          val json = vehicularTranshipmentJson(vehicularTranshipment)
          json.validate[VehicularTranshipment] mustEqual JsSuccess(vehicularTranshipment)
      }
    }

    "must serialise" in {

      forAll(arbitrary[VehicularTranshipment]) {
        vehicularTranshipment =>
          val json = vehicularTranshipmentJson(vehicularTranshipment)
          Json.toJson(vehicularTranshipment)(VehicularTranshipment.writes) mustEqual json
      }
    }
  }

  "Transhipment" - {

    "must deserialise to a Vehicular transhipment" in {

      forAll(arbitrary[VehicularTranshipment]) {
        vehicularTranshipment =>
          val json = vehicularTranshipmentJson(vehicularTranshipment)
          json.validate[Transhipment] mustEqual JsSuccess(vehicularTranshipment)
      }
    }

    "must deserialise to a Container transhipment" in {

      forAll(arbitrary[ContainerTranshipment]) {
        containerTranshipment =>
          val json = containerTranshipmentJson(containerTranshipment)
          json.validate[Transhipment] mustEqual JsSuccess(containerTranshipment)

      }
    }

    "must serialise from a Vehicular transhipment" in {

      forAll(arbitrary[VehicularTranshipment]) {
        vehicularTranshipment =>
          val json = vehicularTranshipmentJson(vehicularTranshipment)
          Json.toJson(vehicularTranshipment: Transhipment)(Transhipment.writes) mustEqual json
      }
    }

    "must serialise from a Container transhipment" in {

      forAll(arbitrary[ContainerTranshipment]) {
        containerTranshipment =>
          val json = containerTranshipmentJson(containerTranshipment)
          Json.toJson(containerTranshipment: Transhipment)(Transhipment.writes) mustEqual json
      }
    }
  }

  "EventDetails" - {

    "must deserialise to an Incident" in {

      forAll(arbitrary[Incident]) {
        incident =>
          val json = incidentJson(incident)
          json.validate[EventDetails] mustEqual JsSuccess(incident)
      }
    }

    "must deserialise to a Vehicular transhipment" in {

      forAll(arbitrary[VehicularTranshipment]) {
        vehicularTranshipment =>
          val json = vehicularTranshipmentJson(vehicularTranshipment)
          json.validate[EventDetails] mustEqual JsSuccess(vehicularTranshipment)
      }
    }

    "must deserialise to a Container transhipment" in {

      forAll(arbitrary[ContainerTranshipment]) {
        containerTranshipment =>
          val json = containerTranshipmentJson(containerTranshipment)
          json.validate[EventDetails] mustEqual JsSuccess(containerTranshipment)
      }
    }

    "must serialise from an Incident" in {

      forAll(arbitrary[Incident]) {
        incident =>
          val json = incidentJson(incident)
          Json.toJson(incident: EventDetails) mustEqual json
      }
    }

    "must serialise from a Vehicular transhipment" in {

      forAll(arbitrary[VehicularTranshipment]) {
        vehicularTranshipment =>
          val json = vehicularTranshipmentJson(vehicularTranshipment)
          Json.toJson(vehicularTranshipment: EventDetails) mustEqual json
      }
    }

    "must serialise from a Container transhipment" in {

      forAll(arbitrary[ContainerTranshipment]) {
        containerTranshipment =>
          val json = containerTranshipmentJson(containerTranshipment)
          Json.toJson(containerTranshipment: EventDetails) mustEqual json
      }
    }
  }

  private def incidentJson(incident: Incident): JsObject = {
    val information = incident.information match {
      case Some(information) =>
        Json.obj("information" -> information)
      case _ =>
        JsObject.empty
    }

    information ++ Json.obj("endorsement" -> Json.toJson(incident.endorsement))
  }

  private def containerTranshipmentJson(containerTranshipment: ContainerTranshipment): JsObject =
    Json.obj(
      "endorsement" -> Json.toJson(containerTranshipment.endorsement),
      "containers"  -> Json.toJson(containerTranshipment.containers)
    )

  private def vehicularTranshipmentJson(vehicularTranshipment: VehicularTranshipment): JsObject =
    Json.obj(
      "transportIdentity" -> vehicularTranshipment.transportIdentity,
      "transportCountry"  -> vehicularTranshipment.transportCountry,
      "endorsement"       -> Json.toJson(vehicularTranshipment.endorsement)
    ) ++ {
      vehicularTranshipment.containers match {
        case Some(containers) =>
          Json.obj("containers" -> containers)
        case _ =>
          JsObject.empty
      }
    }

}
