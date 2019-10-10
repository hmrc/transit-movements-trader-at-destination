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

package models

import generators.ModelGenerators
import models.behaviours.JsonBehaviours
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsSuccess, Json}

class EventDetailsSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with JsonBehaviours {

  "Incident" - {

    mustHaveDualReadsAndWrites(arbitrary[Incident])

    "must deserialise" in {

      forAll(arbitrary[Option[String]], arbitrary[Endorsement]) {
        (information, endorsement) =>

          val json = information.map(
            info =>
              Json.obj(
                "information" -> info,
                "endorsement" -> Json.toJson(endorsement)
              )
          ).getOrElse(
            Json.obj(
              "endorsement" -> Json.toJson(endorsement)
            )
          )

          json.validate[Incident] mustEqual JsSuccess(Incident(information, endorsement))
      }
    }

    "must serialise" in {

      forAll(arbitrary[Option[String]], arbitrary[Endorsement]) {
        (information, endorsement) =>

          val json = information.map(
            info =>
              Json.obj(
                "information" -> info,
                "endorsement" -> Json.toJson(endorsement)
              )
          ).getOrElse(
            Json.obj(
              "endorsement" -> Json.toJson(endorsement)
            )
          )

          Json.toJson(Incident(information, endorsement)) mustEqual json
      }
    }
  }

  "Container transhipment" - {

    mustHaveDualReadsAndWrites(arbitrary[ContainerTranshipment])

    "must fail to construct when given an empty sequence of containers" in {

      forAll(arbitrary[Endorsement]) {
        endorsement =>

          intercept[IllegalArgumentException] {
            ContainerTranshipment(endorsement, Seq.empty)
          }
      }
    }

    "must deserialise" in {

      forAll(arbitrary[Endorsement], arbitrary[Seq[String]]) {
        (endorsement, containers) =>

          whenever(containers.nonEmpty) {

            val json = Json.obj(
              "endorsement" -> Json.toJson(endorsement),
              "containers"  -> Json.toJson(containers)
            )

            val expectedResult = ContainerTranshipment(endorsement, containers)

            json.validate[ContainerTranshipment] mustEqual JsSuccess(expectedResult)
          }
      }
    }

    "must serialise" in {

      forAll(arbitrary[Endorsement], arbitrary[Seq[String]]) {
        (endorsement, containers) =>

          whenever(containers.nonEmpty) {

            val json = Json.obj(
              "endorsement" -> Json.toJson(endorsement),
              "containers"  -> Json.toJson(containers)
            )

            val transhipment = ContainerTranshipment(endorsement, containers)

            Json.toJson(transhipment) mustEqual json
          }
      }
    }
  }

  "Vehicular transhipment" - {

    mustHaveDualReadsAndWrites(arbitrary[VehicularTranshipment])

    "must deserialise when no containers are present" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement])  {
        (id, country, endorsement) =>

          val json = Json.obj(
            "transportIdentity" -> id,
            "transportCountry"  -> country,
            "endorsement"       -> Json.toJson(endorsement)
          )

          val expectedResult = VehicularTranshipment(id, country, endorsement, Seq.empty)

          json.validate[VehicularTranshipment] mustEqual JsSuccess(expectedResult)
      }
    }

    "must deserialise when containers are present" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement], arbitrary[Seq[String]])  {
        (id, country, endorsement, containers) =>

          val json = Json.obj(
            "transportIdentity" -> id,
            "transportCountry"  -> country,
            "endorsement"       -> Json.toJson(endorsement),
            "containers"        -> Json.toJson(containers)
          )

          val expectedResult = VehicularTranshipment(id, country, endorsement, containers)

          json.validate[VehicularTranshipment] mustEqual JsSuccess(expectedResult)
      }
    }

    "must serialise" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement], arbitrary[Seq[String]])  {
        (id, country, endorsement, containers) =>

          val json = if (containers.isEmpty) {
            Json.obj(
              "transportIdentity" -> id,
              "transportCountry"  -> country,
              "endorsement"       -> Json.toJson(endorsement)
            )
          } else {
            Json.obj(
              "transportIdentity" -> id,
              "transportCountry"  -> country,
              "endorsement"       -> Json.toJson(endorsement),
              "containers"        -> Json.toJson(containers)
            )
          }

          val transhipment = VehicularTranshipment(id, country, endorsement, containers)

          Json.toJson(transhipment)(VehicularTranshipment.writes) mustEqual json
      }
    }
  }

  "Transhipment" - {

    "must deserialise to a Vehicular transhipment" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement], arbitrary[Seq[String]])  {
        (id, country, endorsement, containers) =>

          val json = Json.obj(
            "transportIdentity" -> id,
            "transportCountry"  -> country,
            "endorsement"       -> Json.toJson(endorsement),
            "containers"        -> Json.toJson(containers)
          )

          val expectedResult = VehicularTranshipment(id, country, endorsement, containers)

          json.validate[Transhipment] mustEqual JsSuccess(expectedResult)
      }
    }

    "must deserialise to a Container transhipment" in {

      forAll(arbitrary[Endorsement], arbitrary[Seq[String]]) {
        (endorsement, containers) =>

          whenever(containers.nonEmpty) {

            val json = Json.obj(
              "endorsement" -> Json.toJson(endorsement),
              "containers"  -> Json.toJson(containers)
            )

            val expectedResult = ContainerTranshipment(endorsement, containers)

            json.validate[Transhipment] mustEqual JsSuccess(expectedResult)
          }
      }
    }

    "must serialise from a Vehicular transhipment" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement], arbitrary[Seq[String]])  {
        (id, country, endorsement, containers) =>

          val json = if (containers.isEmpty) {
            Json.obj(
              "transportIdentity" -> id,
              "transportCountry"  -> country,
              "endorsement"       -> Json.toJson(endorsement)
            )
          } else {
            Json.obj(
              "transportIdentity" -> id,
              "transportCountry"  -> country,
              "endorsement"       -> Json.toJson(endorsement),
              "containers"        -> Json.toJson(containers)
            )
          }

          val transhipment = VehicularTranshipment(id, country, endorsement, containers)

          Json.toJson(transhipment: Transhipment)(Transhipment.writes) mustEqual json
      }
    }

    "must serialise from a Container transhipment" in {

      forAll(arbitrary[Endorsement], arbitrary[Seq[String]]) {
        (endorsement, containers) =>

          whenever(containers.nonEmpty) {

            val json = Json.obj(
              "endorsement" -> Json.toJson(endorsement),
              "containers"  -> Json.toJson(containers)
            )

            val transhipment = ContainerTranshipment(endorsement, containers)

            Json.toJson(transhipment: Transhipment)(Transhipment.writes) mustEqual json
          }
      }
    }
  }

  "EventDetails" - {

    "must deserialise to an Incident" in {

      forAll(arbitrary[Option[String]], arbitrary[Endorsement]) {
        (information, endorsement) =>

          val json = information.map(
            info =>
              Json.obj(
                "information" -> info,
                "endorsement" -> Json.toJson(endorsement)
              )
          ).getOrElse(
            Json.obj(
              "endorsement" -> Json.toJson(endorsement)
            )
          )

          json.validate[EventDetails] mustEqual JsSuccess(Incident(information, endorsement))
      }
    }

    "must deserialise to a Vehicular transhipment" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement], arbitrary[Seq[String]])  {
        (id, country, endorsement, containers) =>

          val json = Json.obj(
            "transportIdentity" -> id,
            "transportCountry"  -> country,
            "endorsement"       -> Json.toJson(endorsement),
            "containers"        -> Json.toJson(containers)
          )

          val expectedResult = VehicularTranshipment(id, country, endorsement, containers)

          json.validate[EventDetails] mustEqual JsSuccess(expectedResult)
      }
    }

    "must deserialise to a Container transhipment" in {

      forAll(arbitrary[Endorsement], arbitrary[Seq[String]]) {
        (endorsement, containers) =>

          whenever(containers.nonEmpty) {

            val json = Json.obj(
              "endorsement" -> Json.toJson(endorsement),
              "containers"  -> Json.toJson(containers)
            )

            val expectedResult = ContainerTranshipment(endorsement, containers)

            json.validate[EventDetails] mustEqual JsSuccess(expectedResult)
          }
      }
    }

    "must serialise from an Incident" in {

      forAll(arbitrary[Option[String]], arbitrary[Endorsement]) {
        (information, endorsement) =>

          val json = information.map(
            info =>
              Json.obj(
                "information" -> info,
                "endorsement" -> Json.toJson(endorsement)
              )
          ).getOrElse(
            Json.obj(
              "endorsement" -> Json.toJson(endorsement)
            )
          )

          Json.toJson(Incident(information, endorsement): EventDetails) mustEqual json
      }
    }

    "must serialise from a Vehicular transhipment" in {

      forAll(arbitrary[String], arbitrary[String], arbitrary[Endorsement], arbitrary[Seq[String]])  {
        (id, country, endorsement, containers) =>

          val json = if (containers.isEmpty) {
            Json.obj(
              "transportIdentity" -> id,
              "transportCountry"  -> country,
              "endorsement"       -> Json.toJson(endorsement)
            )
          } else {
            Json.obj(
              "transportIdentity" -> id,
              "transportCountry"  -> country,
              "endorsement"       -> Json.toJson(endorsement),
              "containers"        -> Json.toJson(containers)
            )
          }

          val transhipment = VehicularTranshipment(id, country, endorsement, containers)

          Json.toJson(transhipment: EventDetails) mustEqual json
      }
    }

    "must serialise from a Container transhipment" in {

      forAll(arbitrary[Endorsement], arbitrary[Seq[String]]) {
        (endorsement, containers) =>

          whenever(containers.nonEmpty) {

            val json = Json.obj(
              "endorsement" -> Json.toJson(endorsement),
              "containers"  -> Json.toJson(containers)
            )

            val transhipment = ContainerTranshipment(endorsement, containers)

            Json.toJson(transhipment: EventDetails) mustEqual json
          }
      }
    }
  }
}
