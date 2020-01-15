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
import play.api.libs.json._

sealed trait EventDetails

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

final case class Incident(
  information: Option[String],
  endorsement: Endorsement
) extends EventDetails

object Incident {

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

final case class VehicularTranshipment(
  transportIdentity: String,
  transportCountry: String,
  endorsement: Endorsement,
  containers: Option[Seq[Container]]
) extends Transhipment

object VehicularTranshipment {

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

final case class ContainerTranshipment(
  endorsement: Endorsement,
  containers: Seq[Container]
) extends Transhipment {

  require(containers.nonEmpty, "At least one container number must be provided")
}

object ContainerTranshipment {

  implicit lazy val format: Format[ContainerTranshipment] =
    Json.format[ContainerTranshipment]
}
