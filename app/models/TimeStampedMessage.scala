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

package models

import java.time.LocalDate
import java.time.LocalTime

import models.messages.ArrivalNotificationMessage
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.xml.NodeSeq
import scala.xml.XML

final case class TimeStampedMessageJson(date: LocalDate, time: LocalTime, message: ArrivalNotificationMessage)

object TimeStampedMessageJson {
  implicit val formats: OFormat[TimeStampedMessageJson] = Json.format[TimeStampedMessageJson]
}

final case class TimeStampedMessageXml(date: LocalDate, time: LocalTime, message: NodeSeq)

object TimeStampedMessageXml {

  def unapplyString(arg: TimeStampedMessageXml): Option[(LocalDate, LocalTime, String)] =
    Some((arg.date, arg.time, arg.message.toString))

  implicit val writesTimeStampedMessageXml: OWrites[TimeStampedMessageXml] =
    ((__ \ "date").write[LocalDate] and
      (__ \ "time").write[LocalTime] and
      (__ \ "message").write[String])(unlift(TimeStampedMessageXml.unapplyString))

  implicit val readsTimeStampedMessageXml: Reads[TimeStampedMessageXml] = {
    implicit val reads: Reads[NodeSeq] = new Reads[NodeSeq] {
      override def reads(json: JsValue): JsResult[NodeSeq] = json match {
        case JsString(value) => JsSuccess(XML.loadString(value))
        case _               => JsError("Value cannot be parsed as XML")
      }
    }

    Json.reads[TimeStampedMessageXml]
  }

}
