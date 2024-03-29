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

package utils

import models.ArrivalStatus
import models.MessageType
import models.MessageTypeWithTime
import java.time.LocalDateTime

import scala.annotation.tailrec

object MessageTypeUtils {

  def status(messagesList: List[MessageTypeWithTime]): ArrivalStatus = {
    implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _
    val orderedMessages                                     = messagesList.sortBy(_.dateTime)
    latestArrivalStatus(orderedMessages)
  }

  @tailrec
  private def latestArrivalStatus(orderedMessages: List[MessageTypeWithTime]): ArrivalStatus =
    latestMessageTypeIfAny(orderedMessages) match {
      case Some(MessageType.ArrivalNotification)       => ArrivalStatus.ArrivalSubmitted
      case Some(MessageType.ArrivalRejection)          => ArrivalStatus.ArrivalRejected
      case Some(MessageType.UnloadingPermission)       => ArrivalStatus.UnloadingPermission
      case Some(MessageType.UnloadingRemarks)          => ArrivalStatus.UnloadingRemarksSubmitted
      case Some(MessageType.UnloadingRemarksRejection) => ArrivalStatus.UnloadingRemarksRejected
      case Some(MessageType.GoodsReleased)             => ArrivalStatus.GoodsReleased
      case Some(MessageType.XMLSubmissionNegativeAcknowledgement) =>
        val messageListWithoutNegAck = orderedMessages.filterNot(_.messageType == MessageType.XMLSubmissionNegativeAcknowledgement)
        latestMessageTypeIfAny(messageListWithoutNegAck) match {
          case Some(MessageType.UnloadingRemarks)    => ArrivalStatus.UnloadingRemarksSubmittedNegativeAcknowledgement
          case Some(MessageType.ArrivalNotification) => ArrivalStatus.ArrivalSubmittedNegativeAcknowledgement
          case _                                     => latestArrivalStatus(messageListWithoutNegAck)
        }
      case None => ArrivalStatus.NoStatusFound
    }

  private def latestMessageTypeIfAny(orderedMessages: List[MessageTypeWithTime]): Option[MessageType] =
    if (orderedMessages.isEmpty) None else Some(latestMessageType(orderedMessages))

  private def latestMessageType(orderedMessages: List[MessageTypeWithTime]): MessageType = {
    val latestMessage            = orderedMessages.last
    val messagesWithSameDateTime = orderedMessages.filter(_.dateTime == latestMessage.dateTime)

    if (messagesWithSameDateTime.size == 1) {
      latestMessage.messageType
    } else {
      messagesWithSameDateTime.map(_.messageType).max match {
        case MessageType.ArrivalRejection =>
          if (orderedMessages.count(_.messageType == MessageType.ArrivalNotification) > orderedMessages.count(_.messageType == MessageType.ArrivalRejection)) {
            MessageType.ArrivalNotification
          } else {
            MessageType.ArrivalRejection
          }
        case MessageType.UnloadingRemarksRejection =>
          if (orderedMessages.count(_.messageType == MessageType.UnloadingRemarks) > orderedMessages.count(
                _.messageType == MessageType.UnloadingRemarksRejection)) {
            MessageType.UnloadingRemarks
          } else {
            MessageType.UnloadingRemarksRejection
          }
        case MessageType.XMLSubmissionNegativeAcknowledgement if orderedMessages.count(_.messageType == MessageType.UnloadingRemarks) >= 1 =>
          if (orderedMessages
                .count(_.messageType == MessageType.UnloadingRemarks) > orderedMessages.count(
                _.messageType == MessageType.XMLSubmissionNegativeAcknowledgement)) {
            MessageType.UnloadingRemarks
          } else {
            MessageType.XMLSubmissionNegativeAcknowledgement
          }
        case MessageType.XMLSubmissionNegativeAcknowledgement =>
          if (orderedMessages
                .count(_.messageType == MessageType.ArrivalNotification) > orderedMessages.count(
                _.messageType == MessageType.XMLSubmissionNegativeAcknowledgement
              )) {
            MessageType.ArrivalNotification
          } else {
            MessageType.XMLSubmissionNegativeAcknowledgement
          }
        case value => value
      }
    }
  }
}
