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

package models.response

import java.time.LocalDateTime

import controllers.routes
import models.ArrivalId
import models.MovementMessage
import play.api.libs.json.Json
import play.api.libs.json.Writes
import utils.NodeSeqFormat

import scala.xml.NodeSeq

case class ResponseMovementMessage(location: String, dateTime: LocalDateTime, messageType: String, message: NodeSeq)

object ResponseMovementMessage extends NodeSeqFormat {

  def build(a: ArrivalId, mId: Int, m: MovementMessage) =
    ResponseMovementMessage(routes.MovementsController.getMessage(a, mId).url, m.dateTime, m.messageType.code, m.message)

  implicit lazy val writes: Writes[ResponseMovementMessage] = Json.writes[ResponseMovementMessage]
}
