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

import models.XSDFile.GoodsReleasedXSD
import models.XSDFile.UnloadingPermissionXSD

sealed trait MessageResponse {
  val messageReceived: StateAffectingEvent
  val messageType: MessageType
  val xsdFile: XSDFile
}

case object GoodsReleasedResponse extends MessageResponse {
  override val messageReceived: StateAffectingEvent = MessageReceived.GoodsReleased
  override val messageType: MessageType             = MessageType.GoodsReleased
  override val xsdFile: XSDFile                     = GoodsReleasedXSD
}
case object UnloadingPermissionResponse extends MessageResponse {
  override val messageReceived: StateAffectingEvent = MessageReceived.UnloadingPermission
  override val messageType: MessageType             = MessageType.UnloadingPermission
  override val xsdFile: XSDFile                     = UnloadingPermissionXSD
}
