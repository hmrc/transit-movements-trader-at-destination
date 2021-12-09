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

package audit

import models.ChannelType
import play.api.libs.json.Json
import play.api.libs.json.OWrites

case class AuthenticationDetails(channel: ChannelType, enrolmentType: String)

object AuthenticationDetails {
  implicit val writes: OWrites[AuthenticationDetails] = (details: AuthenticationDetails) => {
    Json.obj(
      "channel"       -> details.channel,
      "enrolmentType" -> details.enrolmentType
    )
  }
}