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

import models.ArrivalStatus._

object StatusTransition {

  val validTransitions = Set(
    // Initialized transitions
    Initialized -> ArrivalSubmitted,
    Initialized -> GoodsReleased,
    Initialized -> UnloadingPermission,
    Initialized -> ArrivalRejected,
    Initialized -> UnloadingRemarksRejected,
    Initialized -> ArrivalXMLSubmissionNegativeAcknowledgement,
    // ArrivalSubmitted transitions
    ArrivalSubmitted -> GoodsReleased,
    ArrivalSubmitted -> UnloadingPermission,
    ArrivalSubmitted -> ArrivalRejected,
    ArrivalSubmitted -> UnloadingRemarksRejected,
    ArrivalSubmitted -> ArrivalXMLSubmissionNegativeAcknowledgement,
    // UnloadingPermission transitions
    UnloadingPermission -> UnloadingPermission,
    UnloadingPermission -> UnloadingRemarksSubmitted,
    UnloadingPermission -> GoodsReleased,
    // GoodsReleased transitions
    GoodsReleased -> GoodsReleased,
    // ArrivalRejected transitions
    ArrivalRejected -> ArrivalRejected,
    ArrivalRejected -> GoodsReleased,
    // ArrivalXMLSubmissionNegativeAcknowledgement transitions
    ArrivalXMLSubmissionNegativeAcknowledgement -> ArrivalXMLSubmissionNegativeAcknowledgement,
    ArrivalXMLSubmissionNegativeAcknowledgement -> ArrivalSubmitted,
    // UnloadingRemarksXMLSubmissionNegativeAcknowledgement transitions
    UnloadingRemarksXMLSubmissionNegativeAcknowledgement -> UnloadingRemarksXMLSubmissionNegativeAcknowledgement,
    UnloadingRemarksXMLSubmissionNegativeAcknowledgement -> UnloadingRemarksSubmitted,
    // UnloadingRemarksSubmitted transitions
    UnloadingRemarksSubmitted -> UnloadingRemarksRejected,
    UnloadingRemarksSubmitted -> UnloadingPermission,
    UnloadingRemarksSubmitted -> UnloadingRemarksXMLSubmissionNegativeAcknowledgement,
    UnloadingRemarksSubmitted -> GoodsReleased,
    // UnloadingRemarksRejected transitions
    UnloadingRemarksRejected -> UnloadingRemarksRejected,
    UnloadingRemarksRejected -> UnloadingRemarksSubmitted,
    UnloadingRemarksRejected -> UnloadingPermission,
    UnloadingRemarksRejected -> GoodsReleased
  )

  /** Mapping of the statuses we can transition to from a given status
    */
  val allowedStatusForTransition = validTransitions
    .groupBy {
      case (from, _) => from
    }
    .mapValues(_.map {
      case (_, to) => to
    })
    .toMap

  /** Mapping of the statuses that are able to transition to a given status
    */
  val requiredStatusForTransition = validTransitions
    .groupBy {
      case (_, to) => to
    }
    .mapValues(_.map {
      case (from, _) => from
    })
    .toMap

  def transitionError(
    currentStatus: ArrivalStatus,
    requiredStatuses: Set[ArrivalStatus],
    targetStatus: Option[ArrivalStatus],
    messageReceived: MessageReceivedEvent
  ): Either[TransitionError, ArrivalStatus] = {
    val messageType = messageReceived.toString

    val requiredStatusesString = requiredStatuses
      .filterNot(targetStatus.contains)
      .map(_.toString)
      .toList
      .sorted
      .mkString(" or ")

    Left(
      TransitionError(
        s"Can only accept this type of message [$messageType] directly after [$requiredStatusesString] messages. Current message state is [${currentStatus.toString}]."
      )
    )
  }

  def targetStatus(currentStatus: ArrivalStatus, messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] =
    messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted          => Right(ArrivalSubmitted)
      case MessageReceivedEvent.ArrivalRejected           => Right(ArrivalRejected)
      case MessageReceivedEvent.UnloadingPermission       => Right(UnloadingPermission)
      case MessageReceivedEvent.UnloadingRemarksSubmitted => Right(UnloadingRemarksSubmitted)
      case MessageReceivedEvent.UnloadingRemarksRejected  => Right(UnloadingRemarksRejected)
      case MessageReceivedEvent.GoodsReleased             => Right(GoodsReleased)
      case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement =>
        currentStatus match {
          case Initialized =>
            Right(ArrivalXMLSubmissionNegativeAcknowledgement)
          case ArrivalSubmitted =>
            Right(ArrivalXMLSubmissionNegativeAcknowledgement)
          case UnloadingRemarksSubmitted =>
            Right(UnloadingRemarksXMLSubmissionNegativeAcknowledgement)
          case ArrivalXMLSubmissionNegativeAcknowledgement =>
            Right(ArrivalXMLSubmissionNegativeAcknowledgement)
          case UnloadingRemarksXMLSubmissionNegativeAcknowledgement =>
            Right(UnloadingRemarksXMLSubmissionNegativeAcknowledgement)
          case _ =>
            transitionError(currentStatus, Set(Initialized, ArrivalSubmitted, UnloadingRemarksSubmitted), None, messageReceived)
        }
    }

  def transition(currentStatus: ArrivalStatus, messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] =
    targetStatus(currentStatus, messageReceived).flatMap {
      transitionToStatus =>
        val allowedFromThisStatus   = allowedStatusForTransition.getOrElse(currentStatus, Set.empty)
        val requiredForTargetStatus = requiredStatusForTransition.getOrElse(transitionToStatus, Set.empty)
        if (allowedFromThisStatus.contains(transitionToStatus))
          Right(transitionToStatus)
        else
          transitionError(currentStatus, requiredForTargetStatus, Some(transitionToStatus), messageReceived)
    }
}
