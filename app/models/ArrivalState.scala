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

sealed trait ArrivalState {
  def transition(messageReceived: MessageReceived): ArrivalState
}

object ArrivalState extends Enumerable.Implicits {
  /*
    007 -> Default state would be ArrivalSubmitted
    025 -> Goods submitted event
   */

  case object Initialized extends ArrivalState {
    override def transition(messageReceived: MessageReceived): ArrivalState = messageReceived match {
      case MessageReceived.ArrivalSubmitted => ArrivalSubmitted
      case MessageReceived.GoodsReleased    => GoodsReleased
    }
  }

  case object ArrivalSubmitted extends ArrivalState {
    override def transition(messageReceived: MessageReceived): ArrivalState = messageReceived match {
      case MessageReceived.ArrivalSubmitted => ArrivalSubmitted
      case MessageReceived.GoodsReleased    => GoodsReleased
    }
  }

  case object GoodsReleased extends ArrivalState {
    override def transition(messageReceived: MessageReceived): ArrivalState = messageReceived match {
      case _ => ???
    }
  }

  val values = Seq(
    Initialized,
    ArrivalSubmitted,
    GoodsReleased
  )

  implicit val enumerable: Enumerable[ArrivalState] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
