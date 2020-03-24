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

import java.time.LocalDate

import models._
import play.api.libs.json._

sealed trait ArrivalNotificationMessage

object ArrivalNotificationMessage {

  implicit lazy val reads: Reads[ArrivalNotificationMessage] = {

    implicit class ReadsWithContravariantOr[A](a: Reads[A]) {

      def or[B >: A](b: Reads[B]): Reads[B] =
        a.map[B](identity).orElse(b)
    }

    implicit def convertToSupertype[A, B >: A](a: Reads[A]): Reads[B] =
      a.map(identity)

    NormalNotificationMessage.reads or
      SimplifiedNotificationMessage.reads
  }

  implicit lazy val writes: Writes[ArrivalNotificationMessage] = Writes {
    case n: NormalNotificationMessage     => Json.toJson(n)(NormalNotificationMessage.writes)
    case s: SimplifiedNotificationMessage => Json.toJson(s)(SimplifiedNotificationMessage.writes)
  }
}

final case class NormalNotificationMessage(
  movementReferenceNumber: String,
  notificationPlace: String,
  notificationDate: LocalDate,
  customsSubPlace: Option[String],
  trader: Trader,
  presentationOffice: String,
  enRouteEvents: Option[Seq[EnRouteEvent]]
) extends ArrivalNotificationMessage

object NormalNotificationMessage {

  object Constants {
    val customsSubPlaceLength    = 17
    val notificationPlaceLength  = 35
    val presentationOfficeLength = 8
    val maxNumberOfEnRouteEvents = 9
  }

  implicit lazy val reads: Reads[NormalNotificationMessage] = {

    import play.api.libs.functional.syntax._

    (__ \ "procedure")
      .read[String]
      .flatMap[String] {
        p =>
          if (p == "normal") {
            Reads(_ => JsSuccess(p))
          } else {
            Reads(_ => JsError("procedure must be `normal`"))
          }
      }
      .andKeep(
        (
          (__ \ "movementReferenceNumber").read[String] and
            (__ \ "notificationPlace").read[String] and
            (__ \ "notificationDate").read[LocalDate] and
            (__ \ "customsSubPlace").readNullable[String] and
            (__ \ "trader").read[Trader] and
            (__ \ "presentationOffice").read[String] and
            (__ \ "enRouteEvents").readNullable[Seq[EnRouteEvent]]
        )(NormalNotificationMessage.apply _)
      )
  }

  implicit lazy val writes: OWrites[NormalNotificationMessage] =
    OWrites[NormalNotificationMessage] {
      notification =>
        Json
          .obj(
            "procedure"               -> JsString("normal"),
            "movementReferenceNumber" -> notification.movementReferenceNumber,
            "notificationPlace"       -> notification.notificationPlace,
            "notificationDate"        -> notification.notificationDate,
            "customsSubPlace"         -> notification.customsSubPlace,
            "trader"                  -> Json.toJson(notification.trader),
            "presentationOffice"      -> notification.presentationOffice,
            "enRouteEvents"           -> Json.toJson(notification.enRouteEvents)
          )
          .filterNulls
    }
}

final case class SimplifiedNotificationMessage(
  movementReferenceNumber: String,
  notificationPlace: String,
  notificationDate: LocalDate,
  approvedLocation: Option[String],
  trader: Trader,
  presentationOffice: String,
  enRouteEvents: Option[Seq[EnRouteEvent]]
) extends ArrivalNotificationMessage {

  val procedure = "simplified"
}

object SimplifiedNotificationMessage {

  object Constants {
    val notificationPlaceLength  = 35
    val approvedLocationLength   = 17
    val presentationOfficeLength = 8
    val maxNumberOfEnRouteEvents = 9
  }

  implicit lazy val reads: Reads[SimplifiedNotificationMessage] = {

    import play.api.libs.functional.syntax._

    (__ \ "procedure")
      .read[String]
      .flatMap[String] {
        p =>
          if (p == "simplified") {
            Reads(_ => JsSuccess(p))
          } else {
            Reads(_ => JsError("procedure must be `simplified`"))
          }
      }
      .andKeep(
        (
          (__ \ "movementReferenceNumber").read[String] and
            (__ \ "notificationPlace").read[String] and
            (__ \ "notificationDate").read[LocalDate] and
            (__ \ "approvedLocation").readNullable[String] and
            (__ \ "trader").read[Trader] and
            (__ \ "presentationOffice").read[String] and
            (__ \ "enRouteEvents").readNullable[Seq[EnRouteEvent]]
        )(SimplifiedNotificationMessage.apply _)
      )
  }

  implicit lazy val writes: OWrites[SimplifiedNotificationMessage] = {
    OWrites[SimplifiedNotificationMessage] {
      notification =>
        Json
          .obj(
            "procedure"               -> Json.toJson(notification.procedure),
            "movementReferenceNumber" -> notification.movementReferenceNumber,
            "notificationPlace"       -> notification.notificationPlace,
            "notificationDate"        -> notification.notificationDate,
            "approvedLocation"        -> notification.approvedLocation,
            "trader"                  -> Json.toJson(notification.trader),
            "presentationOffice"      -> notification.presentationOffice,
            "enRouteEvents"           -> Json.toJson(notification.enRouteEvents)
          )
          .filterNulls
    }
  }
}
