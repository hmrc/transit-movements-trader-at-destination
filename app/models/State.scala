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

sealed trait State {
  def transition(event: StateAffectingEvent): State
}

object State extends Enumerable.Implicits {

  case object PendingSubmission extends State {
    override def transition(event: StateAffectingEvent): State = event match {
      case SubmissionResult.Success            => Submitted
      case SubmissionResult.Failure            => SubmissionFailed
      case MessageReceived.UnloadingPermission => UnloadingPermission
      case MessageReceived.GoodsReleased       => GoodsReleased
    }
  }

  case object Submitted extends State {
    override def transition(event: StateAffectingEvent): State = event match {
      case MessageReceived.GoodsReleased       => GoodsReleased
      case MessageReceived.UnloadingPermission => UnloadingPermission
    }
  }

  case object SubmissionFailed extends State {
    override def transition(event: StateAffectingEvent): State = event match {
      case _ => ???
    }
  }

  case object GoodsReleased extends State {
    override def transition(event: StateAffectingEvent): State = event match {
      case _ => ???
    }
  }

  case object UnloadingPermission extends State {
    override def transition(event: StateAffectingEvent): State = event match {
      case _ => ???
    }
  }

  val values = Seq(
    PendingSubmission,
    Submitted,
    SubmissionFailed,
    GoodsReleased,
    UnloadingPermission
  )

  implicit val enumerable: Enumerable[State] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
