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

object StatusTransition {

  def transitionError(
    requiredStatuses: Set[MessageReceivedEvent],
    messageReceived: MessageReceivedEvent
  ): Either[TransitionError, MessageReceivedEvent] = {
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

  //ToDo CTCTRADERS-2634 What do I do with this. Delete it?
  def targetStatus(messageReceived: MessageReceivedEvent): Either[TransitionError, MessageReceivedEvent] =
    messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted                     => Right(messageReceived)
      case MessageReceivedEvent.ArrivalRejected                      => Right(messageReceived)
      case MessageReceivedEvent.UnloadingPermission                  => Right(messageReceived)
      case MessageReceivedEvent.UnloadingRemarksSubmitted            => Right(messageReceived)
      case MessageReceivedEvent.UnloadingRemarksRejected             => Right(messageReceived)
      case MessageReceivedEvent.GoodsReleased                        => Right(messageReceived)
      case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement => Right(messageReceived)
      case _ =>
        transitionError(Set(MessageReceivedEvent.ArrivalSubmitted, MessageReceivedEvent.UnloadingRemarksSubmitted), messageReceived)
    }

}
