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

sealed trait MovementMessage {
  def dateTime: LocalDateTime
  def messageType: MessageType
  def message: NodeSeq
  def optStatus: Option[MessageStatus]
}

final case class MovementMessageWithStatus(dateTime: LocalDateTime,
                                           messageType: MessageType,
                                           message: NodeSeq,
                                           status: MessageStatus,
                                           messageCorrelationId: Int)
    extends MovementMessage { def optStatus = Some(status) }

final case class MovementMessageWithoutStatus(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, messageCorrelationId: Int)
    extends MovementMessage { def optStatus = None }

object MovementMessage extends NodeSeqFormat with MongoDateTimeFormats {

  implicit lazy val reads: Reads[MovementMessage] = new Reads[MovementMessage] {
    override def reads(json: JsValue): JsResult[MovementMessage] = (json \ "status").toOption match {
      case Some(_) =>
        json.validate[MovementMessageWithStatus]
      case None =>
        json.validate[MovementMessageWithoutStatus]
    }
  }

  implicit lazy val writes: OWrites[MovementMessage] = OWrites {
    case ns: MovementMessageWithStatus    => Json.toJsObject(ns)(MovementMessageWithStatus.formatsMovementMessage)
    case ws: MovementMessageWithoutStatus => Json.toJsObject(ws)(MovementMessageWithoutStatus.formatsMovementMessage)
  }

}

object MovementMessageWithStatus extends NodeSeqFormat with MongoDateTimeFormats {

  implicit val formatsMovementMessage: OFormat[MovementMessageWithStatus] =
    Json.format[MovementMessageWithStatus]
}

object MovementMessageWithoutStatus extends NodeSeqFormat with MongoDateTimeFormats {

  implicit val formatsMovementMessage: OFormat[MovementMessageWithoutStatus] =
    Json.format[MovementMessageWithoutStatus]
}
