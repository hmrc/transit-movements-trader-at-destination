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

  def transitionError(
    requiredStatuses: Set[ArrivalStatus],
    messageReceived: MessageReceivedEvent
  ): Either[TransitionError, ArrivalStatus] = {
    val messageType = messageReceived.toString

    val requiredStatusesString = requiredStatuses
      .map(_.toString)
      .toList
      .sorted
      .mkString(" or ")

    Left(
      TransitionError(
        s"Can only accept this type of message [$messageType] directly after [$requiredStatusesString] messages.]."
      )
    )
  }

  def targetStatus(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] =
    messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted                     => Right(ArrivalSubmitted)
      case MessageReceivedEvent.ArrivalRejected                      => Right(ArrivalRejected)
      case MessageReceivedEvent.UnloadingPermission                  => Right(UnloadingPermission)
      case MessageReceivedEvent.UnloadingRemarksSubmitted            => Right(UnloadingRemarksSubmitted)
      case MessageReceivedEvent.UnloadingRemarksRejected             => Right(UnloadingRemarksRejected)
      case MessageReceivedEvent.GoodsReleased                        => Right(GoodsReleased)
      case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement => Right(XMLSubmissionNegativeAcknowledgement)
      case _ =>
        transitionError(Set(Initialized, ArrivalSubmitted, UnloadingRemarksSubmitted), messageReceived)
    }

}
