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

package utils

import java.time.LocalDateTime

import models.ArrivalStatus
import models.MessageType
import models.MessageTypeWithTime

object MessageTypeUtils {

  def currentArrivalStatus(messagesList: List[MessageTypeWithTime]): ArrivalStatus = {
    implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

    val latestMessage            = messagesList.maxBy(_.dateTime)
    val messagesWithSameDateTime = messagesList.filter(_.dateTime == latestMessage.dateTime)

    val currentMessageType = if (messagesWithSameDateTime.size == 1) {
      latestMessage.messageType
    } else {
      messagesWithSameDateTime.map(_.messageType).max match {
        case MessageType.ArrivalRejection =>
          if (messagesList.count(_.messageType == MessageType.ArrivalNotification) > messagesList.count(_.messageType == MessageType.ArrivalRejection)) {
            MessageType.ArrivalNotification
          } else {
            MessageType.ArrivalRejection
          }
        case MessageType.UnloadingRemarksRejection =>
          if (messagesList.count(_.messageType == MessageType.UnloadingRemarks) > messagesList.count(_.messageType == MessageType.UnloadingRemarksRejection)) {
            MessageType.UnloadingRemarks
          } else {
            MessageType.UnloadingRemarksRejection
          }
        case MessageType.XMLSubmissionNegativeAcknowledgement if messagesList.count(_.messageType == MessageType.UnloadingRemarks) >= 1 =>
          if (messagesList.count(_.messageType == MessageType.UnloadingRemarks) > messagesList.count(
                _.messageType == MessageType.XMLSubmissionNegativeAcknowledgement)) {
            MessageType.UnloadingRemarks
          } else {
            MessageType.XMLSubmissionNegativeAcknowledgement
          }
        case MessageType.XMLSubmissionNegativeAcknowledgement =>
          if (messagesList
                .count(_.messageType == MessageType.ArrivalNotification) > messagesList.count(
                _.messageType == MessageType.XMLSubmissionNegativeAcknowledgement)) {
            MessageType.ArrivalNotification
          } else {
            MessageType.XMLSubmissionNegativeAcknowledgement
          }
        case value => value
      }
    }
    toArrivalStatus(currentMessageType)
  }

  def toArrivalStatus(messageType: MessageType): ArrivalStatus =
    messageType match {
      case MessageType.ArrivalNotification                  => ArrivalStatus.ArrivalSubmitted
      case MessageType.ArrivalRejection                     => ArrivalStatus.ArrivalRejected
      case MessageType.UnloadingPermission                  => ArrivalStatus.UnloadingPermission
      case MessageType.UnloadingRemarks                     => ArrivalStatus.UnloadingRemarksSubmitted
      case MessageType.UnloadingRemarksRejection            => ArrivalStatus.UnloadingRemarksRejected
      case MessageType.GoodsReleased                        => ArrivalStatus.GoodsReleased
      case MessageType.XMLSubmissionNegativeAcknowledgement => ArrivalStatus.XMLSubmissionNegativeAcknowledgement
    }
}
