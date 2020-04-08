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

import play.api.libs.json._

import scala.xml.NodeSeq

import utils.NodeSeqFormat._

sealed trait MovementMessage

final case class MovementMessageWithState(date: LocalDate, time: LocalTime, messageType: MessageType, message: NodeSeq, state: MessageState) extends MovementMessage

final case class MovementMessageWithoutState(date: LocalDate, time: LocalTime, messageType: MessageType, message: NodeSeq) extends MovementMessage

object MovementMessage {
  implicit lazy val reads: Reads[MovementMessage] = new Reads[MovementMessage] {
    override def reads(json: JsValue): JsResult[MovementMessage] = (json \ "state").toOption match {
      case Some(_) =>
        json.validate[MovementMessageWithState]
      case None =>
        json.validate[MovementMessageWithoutState]
    }
  }

  implicit lazy val writes: Writes[MovementMessage] = Writes {
    case ns: MovementMessageWithState     => Json.toJson(ns)(MovementMessageWithState.formatsMovementMessage)
    case ws: MovementMessageWithoutState  => Json.toJson(ws)(MovementMessageWithoutState.formatsMovementMessage)
  }
}

object MovementMessageWithState {

  implicit val formatsMovementMessage: OFormat[MovementMessageWithState] =
    Json.format[MovementMessageWithState]
}

object MovementMessageWithoutState {

  implicit val formatsMovementMessage: OFormat[MovementMessageWithoutState] =
    Json.format[MovementMessageWithoutState]
}