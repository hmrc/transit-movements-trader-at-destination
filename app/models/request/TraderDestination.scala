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

package models.request

import helpers.XmlBuilderHelper

import scala.xml.Node

case class TraderDestination(name: Option[String],
                             streetAndNumber: Option[String],
                             postCode: Option[String],
                             city: Option[String],
                             countryCode: Option[String],
                             eori: Option[String])
    extends XmlBuilderHelper {

  def toXml: Node =
    <TRADESTRD>
    {
      buildOptionalElem(name, "NamTRD7") ++
      buildOptionalElem(streetAndNumber, "StrAndNumTRD22") ++
      buildOptionalElem(postCode, "PosCodTRD23") ++
      buildOptionalElem(city, "CitTRD24") ++
      buildOptionalElem(countryCode, "CouTRD25") ++
      buildAndEncodeElem(TraderDestination.Constants.languageCode, "NADLNGRD") ++
      buildOptionalElem(eori, "TINTRD59")
    }
    </TRADESTRD>

}

object TraderDestination {

  object Constants {
    val languageCode: LanguageCode = LanguageCodeEnglish
    val eoriLength                 = 17
    val nameLength                 = 35
    val streetAndNumberLength      = 35
    val postCodeLength             = 9
    val cityLength                 = 35
    val countryCodeLength          = 2
  }
}
