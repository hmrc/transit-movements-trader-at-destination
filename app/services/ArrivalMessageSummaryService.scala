/*
 * Copyright 2022 HM Revenue & Customs
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

import cats._
import cats.data._
import cats.implicits._
import models.MessageType._
import models.Arrival
import models.ArrivalStatus
import models.MessageId
import models.MessageType
import models.MessagesSummary
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus

class ArrivalMessageSummaryService {

  import ArrivalMessageSummaryService._

  private[services] val arrivalNotificationR: Reader[Arrival, (MovementMessage, MessageId)] =
    Reader[Arrival, (MovementMessage, MessageId)](
      _.messagesWithId match {
        case NonEmptyList(arrivalNotification, Nil) =>
          arrivalNotification

        case NonEmptyList(arrivalNotification, _ :: Nil) =>
          arrivalNotification

        case NonEmptyList((msg: MovementMessageWithStatus, id), tail) if msg.messageType == ArrivalNotification =>
          // This is a workaround since we cannot infer the type of head
          // to be (MovementMessageWithStatus, MessageId) using @ in the pattern match
          val head: (MovementMessageWithStatus, MessageId) = (msg, id)

          tail
            .foldLeft(NonEmptyList.of(head))({
              case (acc, (m: MovementMessageWithStatus, mid)) if m.messageType == ArrivalNotification => acc :+ Tuple2(m, mid)
              case (acc, _)                                                                           => acc
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
    )

  private[services] val arrivalRejectionR: Reader[Arrival, Option[(MovementMessage, MessageId)]] =
    Reader[Arrival, Option[(MovementMessage, MessageId)]] {
      arrival =>
        val rejectionNotifications = getLatestMessageWithoutStatus(arrival.messagesWithId)(ArrivalRejection)

        val rejectionNotificationCount = rejectionNotifications.length

        if (rejectionNotificationCount > 0 && arrivalNotificationCount(arrival.messages) == rejectionNotificationCount)
          Some(rejectionNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val xmlSubmissionNegativeAcknowledgementR: Reader[Arrival, Option[(MovementMessage, MessageId)]] =
    Reader[Arrival, Option[(MovementMessage, MessageId)]] {
      arrival =>
        val negativeAcknowledgements = getLatestMessageWithoutStatus(arrival.messagesWithId)(XMLSubmissionNegativeAcknowledgement)

        val negativeAcknowledgementCount = negativeAcknowledgements.length

        if (negativeAcknowledgementCount > 0 && arrivalNotificationCount(arrival.messages) == negativeAcknowledgementCount)
          Some(negativeAcknowledgements.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val unloadingPermissionR: Reader[Arrival, Option[(MovementMessage, MessageId)]] =
    Reader[Arrival, Option[(MovementMessage, MessageId)]] {
      arrival =>
        val unloadingPermissions = getLatestMessageWithoutStatus(arrival.messagesWithId)(UnloadingPermission)

        val unloadingPermissionCount = unloadingPermissions.length

        if (unloadingPermissionCount > 0 && arrivalNotificationCount(arrival.messages) > 0)
          Some(unloadingPermissions.minBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val unloadingRemarksR: Reader[Arrival, Option[(MovementMessage, MessageId)]] =
    Reader[Arrival, Option[(MovementMessage, MessageId)]] {
      arrival =>
        if (unloadingRemarksCount(arrival.messages) > 0) {

          val unloadingRemarks = arrival.messagesWithId
            .foldLeft(Seq.empty[(MovementMessageWithStatus, MessageId)]) {
              case (acc, (m: MovementMessageWithStatus, mid)) if m.messageType == UnloadingRemarks => acc :+ Tuple2(m, mid)
              case (acc, _)                                                                        => acc
            }

          Some(unloadingRemarks.maxBy(_._1.messageCorrelationId))

        } else
          None
    }

  private[services] val unloadingRemarksRejectionsR: Reader[Arrival, Option[(MovementMessage, MessageId)]] =
    Reader[Arrival, Option[(MovementMessage, MessageId)]] {
      arrival =>
        val rejectionNotifications = getLatestMessageWithoutStatus(arrival.messagesWithId)(UnloadingRemarksRejection)

        val rejectionNotificationCount = rejectionNotifications.length

        if (rejectionNotificationCount > 0 && arrival.status == ArrivalStatus.UnloadingRemarksRejected)
          Some(rejectionNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  def arrivalMessagesSummary(arrival: Arrival): MessagesSummary =
    (for {
      arrivalNotification                  <- arrivalNotificationR
      arrivalRejection                     <- arrivalRejectionR
      unloadingPermission                  <- unloadingPermissionR
      unloadingRemarks                     <- unloadingRemarksR
      unloadingRejections                  <- unloadingRemarksRejectionsR
      xmlSubmissionNegativeAcknowledgement <- xmlSubmissionNegativeAcknowledgementR
    } yield {
      MessagesSummary(
        arrival = arrival,
        arrivalNotification = arrivalNotification._2,
        arrivalRejection = arrivalRejection.map(_._2),
        unloadingPermission = unloadingPermission.map(_._2),
        unloadingRemarks = unloadingRemarks.map(_._2),
        unloadingRemarksRejection = unloadingRejections.map(_._2),
        xmlSubmissionNegativeAcknowledgement = xmlSubmissionNegativeAcknowledgement.map(_._2)
      )
    }).run(arrival)

}

object ArrivalMessageSummaryService {
  private val arrivalNotificationCount: NonEmptyList[MovementMessage] => Int = {
    movementMessages =>
      movementMessages.toList.count {
        case m: MovementMessageWithStatus if m.messageType == ArrivalNotification => true
        case _                                                                    => false
      }
  }

  private val unloadingRemarksCount: NonEmptyList[MovementMessage] => Int = {
    movementMessages =>
      movementMessages.toList.count {
        case m: MovementMessageWithStatus if m.messageType == UnloadingRemarks => true
        case _                                                                 => false
      }
  }

  private val getLatestMessageWithoutStatus: NonEmptyList[(MovementMessage, MessageId)] => MessageType => Seq[(MovementMessageWithoutStatus, MessageId)] = {
    messagesWithId => messageType =>
      messagesWithId
        .foldLeft(Seq.empty[(MovementMessageWithoutStatus, MessageId)]) {
          case (acc, (m: MovementMessageWithoutStatus, mid)) if m.messageType == messageType => acc :+ Tuple2(m, mid)
          case (acc, _)                                                                      => acc
        }
  }
}
