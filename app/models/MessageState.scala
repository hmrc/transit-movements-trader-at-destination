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

sealed trait MessageState {
  def transition(event: SubmissionResult): MessageState
}

object MessageState extends Enumerable.Implicits {

  case object SubmissionPending extends MessageState {
    override def transition(event: SubmissionResult): MessageState = event match {
      case SubmissionResult.Success => SubmissionSucceeded
      case _                        => SubmissionFailed
    }
  }

  case object SubmissionFailed extends MessageState {
    override def transition(event: SubmissionResult): MessageState = event match {
      case SubmissionResult.Success => SubmissionSucceeded
      case _                        => SubmissionFailed
    }
  }

  case object SubmissionSucceeded extends MessageState {
    override def transition(event: SubmissionResult): MessageState = SubmissionSucceeded
  }

  val values = Seq(
    SubmissionPending,
    SubmissionFailed,
    SubmissionSucceeded
  )

  implicit val enumerable: Enumerable[MessageState] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
