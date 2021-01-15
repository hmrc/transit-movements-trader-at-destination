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

import java.time.LocalDateTime

import logging.Logging
import org.json.XML
import play.api.libs.json.Json
import play.api.libs.json._
import utils.NodeSeqFormat

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml.NodeSeq

sealed trait MovementMessage {
  def dateTime: LocalDateTime
  def messageType: MessageType
  def message: NodeSeq
  def optStatus: Option[MessageStatus]
  def messageJson: JsObject
}

sealed trait XmlToJson extends Logging {
  protected def toJson(xml: NodeSeq): JsObject =
    Try(Json.parse(XML.toJSONObject(xml.toString).toString).as[JsObject]) match {
      case Success(data) => data
      case Failure(error) =>
        logger.error(s"Failed to convert xml to json with error: ${error.getMessage}")
        Json.obj()
    }
}

final case class MovementMessageWithStatus(dateTime: LocalDateTime,
                                           messageType: MessageType,
                                           message: NodeSeq,
                                           status: MessageStatus,
                                           messageCorrelationId: Int,
                                           messageJson: JsObject)
    extends MovementMessage { def optStatus: Option[MessageStatus] = Some(status) }

final case class MovementMessageWithoutStatus(dateTime: LocalDateTime,
                                              messageType: MessageType,
                                              message: NodeSeq,
                                              messageCorrelationId: Int,
                                              messageJson: JsObject)
    extends MovementMessage { def optStatus: Option[MessageStatus] = None }

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

object MovementMessageWithStatus extends NodeSeqFormat with MongoDateTimeFormats with XmlToJson {

  val writesMovementMessage: OWrites[MovementMessageWithStatus] =
    Json.writes[MovementMessageWithStatus]

  val readsMovementMessage: Reads[MovementMessageWithStatus] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "status").read[MessageStatus] and
        (__ \ "messageCorrelationId").read[Int] and
        (__ \ "messageJson").read[JsObject].orElse(Reads.pure(Json.obj()))
    )(MovementMessageWithStatus(_, _, _, _, _, _))
  }

  implicit val formatsMovementMessage: OFormat[MovementMessageWithStatus] =
    OFormat(readsMovementMessage, writesMovementMessage)

  def apply(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, status: MessageStatus, messageCorrelationId: Int): MovementMessageWithStatus =
    MovementMessageWithStatus(dateTime, messageType, message, status, messageCorrelationId, toJson(message))
}

object MovementMessageWithoutStatus extends NodeSeqFormat with MongoDateTimeFormats with XmlToJson {

  val writesMovementMessage: OWrites[MovementMessageWithoutStatus] =
    Json.writes[MovementMessageWithoutStatus]

  val readsMovementMessage: Reads[MovementMessageWithoutStatus] = {

    import play.api.libs.functional.syntax._

    (
      (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "messageCorrelationId").read[Int] and
        (__ \ "messageJson").read[JsObject].orElse(Reads.pure(Json.obj()))
    )(MovementMessageWithoutStatus(_, _, _, _, _))
  }

  implicit val formatsMovementMessage: OFormat[MovementMessageWithoutStatus] =
    OFormat(readsMovementMessage, writesMovementMessage)

  def apply(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, messageCorrelationId: Int): MovementMessageWithoutStatus =
    MovementMessageWithoutStatus(dateTime, messageType, message, messageCorrelationId, toJson(message))
}
