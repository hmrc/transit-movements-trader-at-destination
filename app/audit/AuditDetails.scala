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

package audit

import models.ChannelType
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.OWrites

case class AuditDetails(channel: ChannelType, json: JsObject)

object AuditDetails {
  implicit val writes: OWrites[AuditDetails] = (details: AuditDetails) => {
    JsObject(Map("channel" -> JsString(details.channel.toString)) ++ details.json.value)
  }
}
