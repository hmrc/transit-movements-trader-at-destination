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

package models

sealed trait MessageReceivedEvent

object MessageReceivedEvent {

  case object ArrivalSubmitted                     extends MessageReceivedEvent
  case object ArrivalRejected                      extends MessageReceivedEvent
  case object UnloadingPermission                  extends MessageReceivedEvent
  case object UnloadingRemarksSubmitted            extends MessageReceivedEvent
  case object UnloadingRemarksRejected             extends MessageReceivedEvent
  case object GoodsReleased                        extends MessageReceivedEvent
  case object XMLSubmissionNegativeAcknowledgement extends MessageReceivedEvent

  val values: Seq[MessageReceivedEvent] = Seq(
    ArrivalSubmitted,
    ArrivalRejected,
    UnloadingPermission,
    UnloadingRemarksSubmitted,
    GoodsReleased,
    UnloadingRemarksRejected,
    XMLSubmissionNegativeAcknowledgement
  )
}
