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

package models.messages

import helpers.XmlBuilderHelper
import models.request.LanguageCode
import models.request.LanguageCodeEnglish
import play.api.libs.json.Json
import play.api.libs.json.OFormat

import scala.xml.Node

case class Seal(numberOrMark: String) extends XmlBuilderHelper {

  def toXml: Node =
    <SEAIDSI1>
      {
        buildAndEncodeElem(numberOrMark, "SeaIdeSI11") ++
        buildAndEncodeElem(Seal.Constants.languageCode, "SeaIdeSI11LNG")
      }
    </SEAIDSI1>

}

object Seal {

  object Constants {
    val numberOrMarkLength         = 20
    val languageCode: LanguageCode = LanguageCodeEnglish
  }

  implicit val format: OFormat[Seal] = Json.format[Seal]
}
