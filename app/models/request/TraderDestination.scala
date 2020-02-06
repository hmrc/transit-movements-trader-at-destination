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

import services.XmlBuilderService

import scala.xml.Node

case class TraderDestination(name: Option[String],
                             streetAndNumber: Option[String],
                             postCode: Option[String],
                             city: Option[String],
                             countryCode: Option[String],
                             eori: Option[String])
    extends XmlBuilderService {

  private val nameTag            = "NamTRD7"
  private val streetAndNumberTag = "StrAndNumTRD22"
  private val postCodeTag        = "PosCodTRD23"
  private val cityTag            = "CitTRD24"
  private val countryCodeTag     = "CouTRD25"
  private val eoriTag            = "TINTRD59"
  private val languageCodeTag    = "NADLNGRD"

  def toXml: Node =
    <TRADESTRD>
    {
      buildOptionalElem(name, nameTag) ++
      buildOptionalElem(streetAndNumber, streetAndNumberTag) ++
      buildOptionalElem(postCode, postCodeTag) ++
      buildOptionalElem(city, cityTag) ++
      buildOptionalElem(countryCode, countryCodeTag) ++
      buildAndEncodeElem(TraderDestination.Constants.languageCode, languageCodeTag) ++
      buildOptionalElem(eori, eoriTag)
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
