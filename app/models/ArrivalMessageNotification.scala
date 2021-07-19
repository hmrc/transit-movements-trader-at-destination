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

import controllers.actions.InboundMessageRequest
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.NodeSeqFormat._

import java.time.LocalDateTime
import scala.xml.NodeSeq
import play.api.http.HeaderNames

case class ArrivalMessageNotification(
  messageUri: String,
  requestId: String,
  arrivalId: ArrivalId,
  messageId: MessageId,
  received: LocalDateTime,
  messageType: MessageType,
  messageBody: Option[NodeSeq]
)

object ArrivalMessageNotification {

  private def requestId(arrivalId: ArrivalId): String =
    s"/customs/transits/movements/arrivals/${arrivalId.index}"

  implicit val writesArrivalId: Writes[ArrivalId] = Writes.of[String].contramap(_.index.toString)

  private val writesArrivalMessageNotification: OWrites[ArrivalMessageNotification] =
    (
      (__ \ "messageUri").write[String] and
        (__ \ "requestId").write[String] and
        (__ \ "arrivalId").write[ArrivalId] and
        (__ \ "messageId").write[String].contramap[MessageId](_.publicValue.toString) and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType] and
        (__ \ "messageBody").writeNullable[NodeSeq]
    )(unlift(ArrivalMessageNotification.unapply))

  implicit val writesArrivalMessageNotificationWithRequestId: OWrites[ArrivalMessageNotification] =
    OWrites.transform(writesArrivalMessageNotification) {
      (arrival, obj) =>
        obj ++ Json.obj("requestId" -> requestId(arrival.arrivalId))
    }

  def fromRequest(request: InboundMessageRequest[NodeSeq], timestamp: LocalDateTime): ArrivalMessageNotification = {
    val oneHundredKilobytes = 100000
    val messageId           = MessageId.fromIndex(request.arrivalRequest.arrival.messages.length)
    val arrivalUrl          = requestId(request.arrivalRequest.arrival.arrivalId)
    val bodySize            = request.headers.get(HeaderNames.CONTENT_LENGTH).map(_.toInt)
    ArrivalMessageNotification(
      s"$arrivalUrl/messages/${messageId.publicValue}",
      arrivalUrl,
      request.arrivalRequest.arrival.arrivalId,
      messageId,
      timestamp,
      request.message.messageType.messageType,
      if (bodySize.exists(_ <= oneHundredKilobytes)) Some(request.body) else None
    )
  }
}
