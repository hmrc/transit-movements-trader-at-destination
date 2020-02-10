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

import models._
import models.request.LanguageCodeEnglish
import play.api.libs.json._
import services.XmlBuilderService

import scala.xml.Node
import scala.xml.NodeSeq

final case class EnRouteEvent(place: String, countryCode: String, alreadyInNcts: Boolean, eventDetails: EventDetails, seals: Option[Seq[Seal]])
    extends XmlBuilderService {

  def toXml: Node = {

    val buildSealsXml = seals match {
      case Some(seals) => {
        <SEAINFSF1>
        {
          buildAndEncodeElem(seals.size.toString, "SeaNumSF12") ++
          seals.map(_.toXml)
        }
        </SEAINFSF1>
      }
      case _ => NodeSeq.Empty
    }

    <ENROUEVETEV>
      {
        buildAndEncodeElem(place,"PlaTEV10") ++
        buildAndEncodeElem(LanguageCodeEnglish.code,"PlaTEV10LNG") ++
        buildAndEncodeElem(countryCode,"CouTEV13")
      }
      <CTLCTL>
        {
        buildAndEncodeElem(alreadyInNcts,"AlrInNCTCTL29")
        }
      </CTLCTL>
      {
        eventDetails match {
          case incident: Incident                           => incident.toXml ++ buildSealsXml
          case containerTranshipment: ContainerTranshipment => buildSealsXml ++ containerTranshipment.toXml
          case vehicularTranshipment: VehicularTranshipment => buildSealsXml ++ vehicularTranshipment.toXml
        }
      }
    </ENROUEVETEV>
  }
}

object EnRouteEvent {

  object Constants {
    val placeLength       = 35
    val countryCodeLength = 2
  }

  implicit lazy val reads: Reads[EnRouteEvent] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "place").read[String] and
        (__ \ "countryCode").read[String] and
        (__ \ "alreadyInNcts").read[Boolean] and
        (__ \ "eventDetails").read[EventDetails] and
        (__ \ "seals").readNullable[Seq[Seal]]
    )(EnRouteEvent(_, _, _, _, _))
  }

  implicit lazy val writes: OWrites[EnRouteEvent] =
    OWrites[EnRouteEvent] {
      event =>
        Json
          .obj(
            "place"         -> event.place,
            "countryCode"   -> event.countryCode,
            "alreadyInNcts" -> event.alreadyInNcts,
            "eventDetails"  -> Json.toJson(event.eventDetails),
            "seals"         -> Json.toJson(event.seals)
          )
          .filterNulls
    }
}
