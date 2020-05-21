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

package services

import cats.data.NonEmptyList
import models.MessageType._
import models.Arrival
import models.MessageId
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus

class ArrivalMessageSummaryService {

  def arrivalNotification(arrival: Arrival): (MovementMessage, MessageId) = arrival.messagesWithId match {
    case NonEmptyList(arrivalNotification, Nil)                                                     => arrivalNotification
    case NonEmptyList(arrivalNotification, _ :: Nil)                                                => arrivalNotification
    case NonEmptyList((msg @ MovementMessageWithStatus(_, ArrivalNotification, _, _, _), id), tail) =>
      // This is a workaround since we cannot infer the type of head
      // to be (MovementMessageWithStatus, MessageId) using @ in the pattern match
      val head: (MovementMessageWithStatus, MessageId) = (msg, id)

      tail
        .foldLeft(NonEmptyList.of(head))({
          case (acc, (m @ MovementMessageWithStatus(_, ArrivalNotification, _, _, _), mid)) => acc :+ Tuple2(m, mid)
          case (acc, _)                                                                     => acc
        })
        .toList
        .maxBy(_._1.messageCorrelationId)

    case NonEmptyList((msg, _), _) =>
      // Unreachable but unprovable
      throw new RuntimeException(
        "Reached an invalid state when summarizing Arrival Notification. " +
          "Expected the first message of the movement to be MovementMessageWithStatus with an ArrivalNotification, " +
          s"but got ${msg.getClass} that contained a ${msg.messageType.code}"
      )
  }

  def arrivalRejection(arrival: Arrival): Option[(MovementMessage, MessageId)] = {

    lazy val arrivalNotificationCount = arrival.messages.toList.count {
      case MovementMessageWithStatus(_, ArrivalNotification, _, _, _) => true
      case _                                                          => false
    }

    val rejectionNotifications = arrival.messagesWithId
      .foldLeft(Seq.empty[(MovementMessageWithoutStatus, MessageId)]) {
        case (acc, (m @ MovementMessageWithoutStatus(_, ArrivalRejection, _, _), mid)) => acc :+ Tuple2(m, mid)
        case (acc, _)                                                                  => acc
      }

    val rejectionNotificationCount = rejectionNotifications.length

    if (rejectionNotificationCount > 0 && arrivalNotificationCount == rejectionNotificationCount)
      Some(rejectionNotifications.maxBy(_._1.messageCorrelationId))
    else
      None

  }

}