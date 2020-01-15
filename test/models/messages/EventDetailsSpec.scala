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
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class EventDetailsSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "Incident" - {

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
