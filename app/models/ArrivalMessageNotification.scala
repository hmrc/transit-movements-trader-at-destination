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

import java.time.LocalDateTime
import scala.xml.NodeSeq

case class ArrivalMessageNotification(
  messageUri: String,
  requestId: String,
  arrivalId: ArrivalId,
  messageId: MessageId,
  received: LocalDateTime,
  messageType: MessageType
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
        (__ \ "messageId").write[MessageId] and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType]
    )(unlift(ArrivalMessageNotification.unapply))

  implicit val writesArrivalMessageNotificationWithRequestId: OWrites[ArrivalMessageNotification] =
    OWrites.transform(writesArrivalMessageNotification) {
      (arrival, obj) =>
        obj ++ Json.obj("requestId" -> requestId(arrival.arrivalId))
    }

  def fromRequest(request: InboundMessageRequest[NodeSeq], timestamp: LocalDateTime): ArrivalMessageNotification = {
    val messageId  = request.arrivalRequest.arrival.nextMessageId
    val arrivalUrl = requestId(request.arrivalRequest.arrival.arrivalId)
    ArrivalMessageNotification(
      s"$arrivalUrl/messages/${messageId.index}",
      arrivalUrl,
      request.arrivalRequest.arrival.arrivalId,
      messageId,
      timestamp,
      request.message.messageType.messageType
    )
  }
}
