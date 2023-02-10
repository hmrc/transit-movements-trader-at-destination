/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.NodeSeqFormat._

import java.time.LocalDateTime
import scala.xml.NodeSeq

case class ArrivalMessageNotification(
  messageUri: String,
  requestId: String,
  customerId: String,
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

  implicit val writesArrivalMessageNotification: OWrites[ArrivalMessageNotification] =
    (
      (__ \ "messageUri").write[String] and
        (__ \ "requestId").write[String] and
        (__ \ "customerId").write[String] and
        (__ \ "arrivalId").write[ArrivalId] and
        (__ \ "messageId").write[MessageId] and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType] and
        (__ \ "messageBody").writeNullable[NodeSeq]
    )(unlift(ArrivalMessageNotification.unapply))

  def fromInboundRequest(inboundMessageRequest: InboundMessageRequest, contentLength: Option[Int]): ArrivalMessageNotification =
    inboundMessageRequest match {
      case InboundMessageRequest(arrival, inboundMessageResponse, movementMessage) =>
        val eightyKilobytes = 80000
        val eoriNumber      = arrival.eoriNumber
        val messageId       = arrival.nextMessageId
        val arrivalUrl      = requestId(arrival.arrivalId)

        ArrivalMessageNotification(
          s"$arrivalUrl/messages/${messageId.value}",
          arrivalUrl,
          eoriNumber,
          arrival.arrivalId,
          messageId,
          movementMessage.dateTime,
          inboundMessageResponse.messageType,
          if (contentLength.exists(_ < eightyKilobytes)) Some(movementMessage.message) else None
        )
    }
}
