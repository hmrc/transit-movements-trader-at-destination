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

package models

import play.api.libs.json.Json
import play.api.libs.json.Writes

sealed trait ArrivalStatus {
  def transition(messageReceived: MessageReceivedEvent): ArrivalStatus
}

object ArrivalStatus extends Enumerable.Implicits {

  case object Initialized extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted         => ArrivalSubmitted
      case MessageReceivedEvent.GoodsReleased            => GoodsReleased
      case MessageReceivedEvent.UnloadingPermission      => UnloadingPermission
      case MessageReceivedEvent.ArrivalRejected          => ArrivalRejected
      case MessageReceivedEvent.UnloadingRemarksRejected => UnloadingRemarksRejected
      case _                                             => throw new Exception(s"Tried to transition from Initialized to $messageReceived.")
    }
  }

  case object ArrivalSubmitted extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted         => ArrivalSubmitted
      case MessageReceivedEvent.GoodsReleased            => GoodsReleased
      case MessageReceivedEvent.UnloadingPermission      => UnloadingPermission
      case MessageReceivedEvent.ArrivalRejected          => ArrivalRejected
      case MessageReceivedEvent.UnloadingRemarksRejected => UnloadingRemarksRejected
      case _                                             => throw new Exception(s"Tried to transition from ArrivalSubmitted to $messageReceived.")
    }
  }

  case object UnloadingPermission extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = messageReceived match {
      case MessageReceivedEvent.UnloadingPermission       => UnloadingPermission
      case MessageReceivedEvent.UnloadingRemarksSubmitted => UnloadingRemarksSubmitted
      case MessageReceivedEvent.GoodsReleased             => GoodsReleased
      case _                                              => throw new Exception(s"Tried to transition from ArrivalSubmitted to $messageReceived.")
    }
  }

  case object GoodsReleased extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = this
  }

  case object ArrivalRejected extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = messageReceived match {
      case MessageReceivedEvent.ArrivalRejected => ArrivalRejected
      case MessageReceivedEvent.GoodsReleased   => GoodsReleased
      case _                                    => throw new Exception(s"Tried to transition from ArrivalRejected to $messageReceived.")
    }
  }

  case object UnloadingRemarksSubmitted extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = messageReceived match {
      case MessageReceivedEvent.UnloadingRemarksSubmitted => UnloadingRemarksSubmitted
      case MessageReceivedEvent.UnloadingRemarksRejected  => UnloadingRemarksRejected
      case MessageReceivedEvent.GoodsReleased             => GoodsReleased
      case _                                              => throw new Exception(s"Tried to transition from UnloadingRemarksSubmitted to $messageReceived.")
    }
  }

  case object UnloadingRemarksRejected extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): ArrivalStatus = messageReceived match {
      case MessageReceivedEvent.UnloadingRemarksRejected  => UnloadingRemarksRejected
      case MessageReceivedEvent.UnloadingRemarksSubmitted => UnloadingRemarksSubmitted
      case MessageReceivedEvent.GoodsReleased             => GoodsReleased
      case _                                              => throw new Exception(s"Tried to transition from UnloadingRemarksRejected to $messageReceived.")
    }
  }

  val values = Seq(
    Initialized,
    ArrivalSubmitted,
    UnloadingPermission,
    UnloadingRemarksSubmitted,
    GoodsReleased,
    ArrivalRejected,
    UnloadingRemarksRejected
  )

  implicit val enumerable: Enumerable[ArrivalStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)

  implicit def arrivalStateUpdate(implicit writes: Writes[ArrivalStatus]): ArrivalModifier[ArrivalStatus] =
    ArrivalModifier(
      value =>
        Json.obj(
          "$set" -> Json.obj(
            "status" -> value
          )))
}
