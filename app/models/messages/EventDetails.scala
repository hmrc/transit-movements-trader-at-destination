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

import helpers.XmlBuilderHelper
import models._
import models.request.Header
import play.api.libs.json._

import scala.xml.Node
import scala.xml.NodeSeq

sealed trait EventDetails {
  def toXml: Node
}

object EventDetails {

  implicit lazy val reads: Reads[EventDetails] = {

    implicit class ReadsWithContravariantOr[A](a: Reads[A]) {

      def or[B >: A](b: Reads[B]): Reads[B] =
        a.map[B](identity).orElse(b)
    }

    implicit def convertToSupertype[A, B >: A](a: Reads[A]): Reads[B] =
      a.map(identity)

    Transhipment.reads or
      Incident.format
  }

  implicit lazy val writes: Writes[EventDetails] = Writes {
    case i: Incident     => Json.toJson(i)(Incident.format)
    case t: Transhipment => Json.toJson(t)(Transhipment.writes)
  }
}

final case class Incident(information: Option[String], endorsement: Endorsement) extends XmlBuilderHelper with EventDetails {

  def toXml: Node =
    <INCINC>
    {
      buildIncidentFlag(information.isDefined) ++
      buildOptionalElem(information, "IncInfINC4") ++
      buildAndEncodeElem(Header.Constants.languageCode, "IncInfINC4LNG") ++
      buildOptionalElem(endorsement.date, "EndDatINC6") ++
      buildOptionalElem(endorsement.authority, "EndAutINC7") ++
      buildAndEncodeElem(Header.Constants.languageCode, "EndAutINC7LNG") ++
      buildOptionalElem(endorsement.place, "EndPlaINC10") ++
      buildAndEncodeElem(Header.Constants.languageCode, "EndPlaINC10LNG") ++
      buildOptionalElem(endorsement.country, "EndCouINC12")
    }
    </INCINC>
}

object Incident {

  object Constants {
    val informationLength = 350
  }

  implicit lazy val format: Format[Incident] =
    Json.format[Incident]
}

sealed trait Transhipment extends EventDetails

object Transhipment {

  implicit lazy val reads: Reads[Transhipment] = {

    implicit class ReadsWithContravariantOr[A](a: Reads[A]) {

      def or[B >: A](b: Reads[B]): Reads[B] =
        a.map[B](identity).orElse(b)
    }

    implicit def convertToSupertype[A, B >: A](a: Reads[A]): Reads[B] =
      a.map(identity)

    VehicularTranshipment.reads or
      ContainerTranshipment.format
  }

  implicit lazy val writes: Writes[Transhipment] = Writes {
    case t: VehicularTranshipment => Json.toJson(t)(VehicularTranshipment.writes)
    case t: ContainerTranshipment => Json.toJson(t)(ContainerTranshipment.format)
  }
}

final case class VehicularTranshipment(transportIdentity: String, transportCountry: String, endorsement: Endorsement, containers: Option[Seq[Container]])
    extends XmlBuilderHelper
    with Transhipment {

  def toXml: Node =
    <TRASHP>
      {
        buildAndEncodeElem(transportIdentity,"NewTraMeaIdeSHP26") ++
        buildAndEncodeElem(Header.Constants.languageCode,"NewTraMeaIdeSHP26LNG") ++
        buildAndEncodeElem(transportCountry,"NewTraMeaNatSHP54") ++
        buildOptionalElem(endorsement.date, "EndDatSHP60") ++
        buildOptionalElem(endorsement.authority, "EndAutSHP61") ++
        buildAndEncodeElem(Header.Constants.languageCode, "EndAutSHP61LNG") ++
        buildOptionalElem(endorsement.place, "EndPlaSHP63") ++
        buildAndEncodeElem(Header.Constants.languageCode, "EndPlaSHP63LNG") ++
        buildOptionalElem(endorsement.country, "EndCouSHP65") ++
        containers.fold(NodeSeq.Empty)(_.map(_.toXml))
      }
    </TRASHP>
}

object VehicularTranshipment {

  object Constants {
    val transportIdentityLength = 27
    val transportCountryLength  = 2
  }

  implicit lazy val reads: Reads[VehicularTranshipment] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "transportIdentity").read[String] and
        (__ \ "transportCountry").read[String] and
        (__ \ "endorsement").read[Endorsement] and
        (__ \ "containers").readNullable[Seq[Container]]
    )(VehicularTranshipment.apply _)
  }

  implicit lazy val writes: OWrites[VehicularTranshipment] =
    OWrites[VehicularTranshipment] {
      transhipment =>
        Json
          .obj(
            "transportIdentity" -> transhipment.transportIdentity,
            "transportCountry"  -> transhipment.transportCountry,
            "endorsement"       -> Json.toJson(transhipment.endorsement),
            "containers"        -> Json.toJson(transhipment.containers)
          )
          .filterNulls
    }
}

final case class ContainerTranshipment(endorsement: Endorsement, containers: Seq[Container]) extends XmlBuilderHelper with Transhipment {

  def toXml: Node =
    <TRASHP> {
        buildOptionalElem(endorsement.date, "EndDatSHP60") ++
        buildOptionalElem(endorsement.authority, "EndAutSHP61") ++
        buildAndEncodeElem(Header.Constants.languageCode,"EndAutSHP61LNG") ++
        buildOptionalElem(endorsement.place, "EndPlaSHP63") ++
        buildAndEncodeElem(Header.Constants.languageCode,"EndPlaSHP63LNG") ++
        buildOptionalElem(endorsement.country, "EndCouSHP65") ++
        containers.map(_.toXml)
      }
    </TRASHP>

  require(containers.nonEmpty, "At least one container number must be provided")
}

object ContainerTranshipment {

  implicit lazy val format: Format[ContainerTranshipment] =
    Json.format[ContainerTranshipment]
}
