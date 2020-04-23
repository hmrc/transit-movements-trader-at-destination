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

sealed trait StateAffectingEvent

sealed trait SubmissionResult extends StateAffectingEvent

object SubmissionResult {

  case object Success extends SubmissionResult
  case object Failure extends SubmissionResult
}

sealed trait MessageReceived extends StateAffectingEvent

object MessageReceived {

  case object GoodsReleased       extends MessageReceived
  case object UnloadingPermission extends MessageReceived
}
