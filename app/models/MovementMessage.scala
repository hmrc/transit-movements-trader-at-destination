/*
 * Copyright 2021 HM Revenue & Customs
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

import logging.Logging
import org.json.XML
import play.api.libs.json.Json
import play.api.libs.json._
import utils.NodeSeqFormat
import utils.XmlToJsonConverter
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneOffset

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml.NodeSeq

sealed trait MovementMessage {
  def messageId: MessageId
  def dateTime: LocalDateTime
  def messageType: MessageType
  def message: NodeSeq
  def optStatus: Option[MessageStatus]
  def messageJson: JsObject
  def messageCorrelationId: Int

  def receivedBefore(requestedDate: OffsetDateTime): Boolean =
    dateTime.atOffset(ZoneOffset.UTC).isBefore(requestedDate)
}

final case class MovementMessageWithStatus private (
  messageId: MessageId,
  dateTime: LocalDateTime,
  messageType: MessageType,
  message: NodeSeq,
  status: MessageStatus,
  messageCorrelationId: Int,
  messageJson: JsObject
) extends MovementMessage { def optStatus: Option[MessageStatus] = Some(status) }

final case class MovementMessageWithoutStatus private (
  messageId: MessageId,
  dateTime: LocalDateTime,
  messageType: MessageType,
  message: NodeSeq,
  messageCorrelationId: Int,
  messageJson: JsObject
) extends MovementMessage { def optStatus: Option[MessageStatus] = None }

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

  val writesMovementMessage: OWrites[MovementMessageWithStatus] =
    Json.writes[MovementMessageWithStatus]

  val readsMovementMessage: Reads[MovementMessageWithStatus] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "messageId").read[MessageId] and
        (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "status").read[MessageStatus] and
        (__ \ "messageCorrelationId").read[Int] and
        (__ \ "messageJson").read[JsObject].orElse(Reads.pure(Json.obj()))
    )(MovementMessageWithStatus(_, _, _, _, _, _, _))
  }

  implicit val formatsMovementMessage: OFormat[MovementMessageWithStatus] =
    OFormat(readsMovementMessage, writesMovementMessage)

  def apply(
    messageId: MessageId,
    dateTime: LocalDateTime,
    messageType: MessageType,
    message: NodeSeq,
    status: MessageStatus,
    messageCorrelationId: Int
  )(xmlToJson: XmlToJsonConverter): MovementMessageWithStatus =
    MovementMessageWithStatus(messageId, dateTime, messageType, message, status, messageCorrelationId, xmlToJson.toJson(message))
}

object MovementMessageWithoutStatus extends NodeSeqFormat with MongoDateTimeFormats {

  val writesMovementMessage: OWrites[MovementMessageWithoutStatus] =
    Json.writes[MovementMessageWithoutStatus]

  val readsMovementMessage: Reads[MovementMessageWithoutStatus] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "messageId").read[MessageId] and
        (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "messageCorrelationId").read[Int] and
        (__ \ "messageJson").read[JsObject].orElse(Reads.pure(Json.obj()))
    )(MovementMessageWithoutStatus(_, _, _, _, _, _))
  }

  implicit val formatsMovementMessage: OFormat[MovementMessageWithoutStatus] =
    OFormat(readsMovementMessage, writesMovementMessage)

  def apply(
    messageId: MessageId,
    dateTime: LocalDateTime,
    messageType: MessageType,
    message: NodeSeq,
    messageCorrelationId: Int
  )(xmlToJson: XmlToJsonConverter): MovementMessageWithoutStatus =
    MovementMessageWithoutStatus(messageId, dateTime, messageType, message, messageCorrelationId, xmlToJson.toJson(message))
}
