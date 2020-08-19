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

package utils

import org.json.JSONException
import org.json.JSONObject
import org.json.XML
import play.api.Logger

object JsonService {

  def fromXml(xml: String): Option[JSONObject] =
    try {
      Some(XML.toJSONObject(xml))
    } catch {
      case error: JSONException =>
        Logger.error(s"Failed to convert xml to json with error: $error")
        None
    }

}
