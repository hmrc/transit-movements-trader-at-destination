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

package models.messages

import java.time.LocalDate

import models._
import play.api.libs.json._

sealed trait ArrivalNotification

object ArrivalNotification {

  implicit lazy val reads: Reads[ArrivalNotification] = {

    implicit class ReadsWithContravariantOr[A](a: Reads[A]) {

      def or[B >: A](b: Reads[B]): Reads[B] =
        a.map[B](identity).orElse(b)
    }

    implicit def convertToSupertype[A, B >: A](a: Reads[A]): Reads[B] =
      a.map(identity)

    NormalNotification.reads or
      SimplifiedNotification.reads
  }

  implicit lazy val writes: Writes[ArrivalNotification] = Writes {
    case n: NormalNotification     => Json.toJson(n)(NormalNotification.writes)
    case s: SimplifiedNotification => Json.toJson(s)(SimplifiedNotification.writes)
  }
}

final case class NormalNotification (
    movementReferenceNumber: String,
    notificationPlace: String,
    notificationDate: LocalDate,
    customsSubPlace: Option[String],
    trader: Trader,
    presentationOffice: String,
    enRouteEvents: Seq[EnRouteEvent]
) extends ArrivalNotification {

  val procedure: ProcedureType = ProcedureType.Normal
}

object NormalNotification {

  implicit lazy val reads: Reads[NormalNotification] = {

    import play.api.libs.functional.syntax._

    (__ \ "procedure").read[String].flatMap[String] {
      p =>
        if (p == ProcedureType.Normal.toString) {
          Reads(_ => JsSuccess(p))
        } else {
          Reads(_ => JsError("procedure must be `normal`"))
        }
    }.andKeep(
      (
        (__ \ "movementReferenceNumber").read[String] and
        (__ \ "notificationPlace").read[String] and
        (__ \ "notificationDate").read[LocalDate] and
        (__ \ "customsSubPlace").readNullable[String] and
        (__ \ "trader").read[Trader] and
        (__ \ "presentationOffice").read[String] and
        ((__ \ "enRouteEvents").read[Seq[EnRouteEvent]] or Reads.pure(Seq[EnRouteEvent]()))
      )(NormalNotification(_, _, _, _, _, _, _))
    )
  }

  implicit lazy val writes: OWrites[NormalNotification] =
    OWrites[NormalNotification] {
      notification =>

        Json.obj(
          "procedure"               -> Json.toJson(notification.procedure),
          "movementReferenceNumber" -> notification.movementReferenceNumber,
          "notificationPlace"       -> notification.notificationPlace,
          "notificationDate"        -> notification.notificationDate,
          "customsSubPlace"         -> notification.customsSubPlace,
          "trader"                  -> Json.toJson(notification.trader),
          "presentationOffice"      -> notification.presentationOffice,
          "enRouteEvents"           -> Json.toJson(notification.enRouteEvents)
        ).filterNulls
    }
}

final case class SimplifiedNotification (
    movementReferenceNumber: String,
    notificationPlace: String,
    notificationDate: LocalDate,
    approvedLocation: Option[String],
    trader: Trader,
    presentationOffice: String,
    enRouteEvents: Seq[EnRouteEvent]
) extends ArrivalNotification {

  val procedure: ProcedureType = ProcedureType.Simplified
}

object SimplifiedNotification {

  implicit lazy val reads: Reads[SimplifiedNotification] = {

    import play.api.libs.functional.syntax._

    (__ \ "procedure").read[String].flatMap[String] {
      p =>
        if (p == ProcedureType.Simplified.toString) {
          Reads(_ => JsSuccess(p))
        } else {
          Reads(_ => JsError("procedure must be `simplified`"))
        }
    }.andKeep(
      (
        (__ \ "movementReferenceNumber").read[String] and
        (__ \ "notificationPlace").read[String] and
        (__ \ "notificationDate").read[LocalDate] and
        (__ \ "approvedLocation").readNullable[String] and
        (__ \ "trader").read[Trader] and
        (__ \ "presentationOffice").read[String] and
        ((__ \ "enRouteEvents").read[Seq[EnRouteEvent]] or Reads.pure(Seq[EnRouteEvent]()))
      )(SimplifiedNotification(_, _, _, _, _, _, _))
    )
  }

  implicit lazy val writes: OWrites[SimplifiedNotification] = {
    OWrites[SimplifiedNotification] {
      notification =>

        Json.obj(
          "procedure"               -> Json.toJson(notification.procedure),
          "movementReferenceNumber" -> notification.movementReferenceNumber,
          "notificationPlace"       -> notification.notificationPlace,
          "notificationDate"        -> notification.notificationDate,
          "approvedLocation"        -> notification.approvedLocation,
          "trader"                  -> Json.toJson(notification.trader),
          "presentationOffice"      -> notification.presentationOffice,
          "enRouteEvents"           -> Json.toJson(notification.enRouteEvents)
        ).filterNulls
    }
  }
}
