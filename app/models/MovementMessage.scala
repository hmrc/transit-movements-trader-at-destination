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

import java.time.LocalDateTime

import play.api.libs.json._
import utils.NodeSeqFormat

import scala.xml.NodeSeq

abstract class MovementMessage {
  def dateTime: LocalDateTime
}

final case class MovementMessageWithState(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, state: MessageState, messageCorrelationId: Int)
    extends MovementMessage

final case class MovementMessageWithoutState(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, messageCorrelationId: Int)
    extends MovementMessage

object MovementMessage extends NodeSeqFormat {
  implicit lazy val reads: Reads[MovementMessage] = new Reads[MovementMessage] {
    override def reads(json: JsValue): JsResult[MovementMessage] = (json \ "state").toOption match {
      case Some(_) =>
        json.validate[MovementMessageWithState]
      case None =>
        json.validate[MovementMessageWithoutState]
    }
  }

  implicit lazy val writes: OWrites[MovementMessage] = OWrites {
    case ns: MovementMessageWithState    => Json.toJsObject(ns)(MovementMessageWithState.formatsMovementMessage)
    case ws: MovementMessageWithoutState => Json.toJsObject(ws)(MovementMessageWithoutState.formatsMovementMessage)
  }

}

object MovementMessageWithState extends NodeSeqFormat {

  implicit val formatsMovementMessage: OFormat[MovementMessageWithState] =
    Json.format[MovementMessageWithState]
}

object MovementMessageWithoutState extends NodeSeqFormat {

  implicit val formatsMovementMessage: OFormat[MovementMessageWithoutState] =
    Json.format[MovementMessageWithoutState]
}
