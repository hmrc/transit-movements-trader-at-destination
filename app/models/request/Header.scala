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

import java.time.LocalDateTime

import helpers.XmlBuilderHelper
import utils.Format

import scala.xml.Node

case class Header(movementReferenceNumber: String,
                  customsSubPlace: Option[String] = None,
                  arrivalNotificationPlace: String,
                  arrivalAgreedLocationOfGoods: Option[String] = None,
                  procedureTypeFlag: ProcedureTypeFlag)
    extends XmlBuilderHelper {

  def toXml(implicit dateTime: LocalDateTime): Node =
    <HEAHEA>
    {
      buildAndEncodeElem(movementReferenceNumber, "DocNumHEA5") ++
      buildOptionalElem(customsSubPlace, "CusSubPlaHEA66") ++
      buildAndEncodeElem(arrivalNotificationPlace, "ArrNotPlaHEA60") ++
      buildAndEncodeElem(Header.Constants.languageCode, "ArrNotPlaHEA60LNG") ++
      buildOptionalElem(arrivalAgreedLocationOfGoods, "ArrAgrLocCodHEA62") ++
      buildOptionalElem(arrivalAgreedLocationOfGoods, "ArrAgrLocOfGooHEA63") ++
      buildAndEncodeElem(Header.Constants.languageCode, "ArrAgrLocOfGooHEA63LNG") ++
      buildOptionalElem(arrivalAgreedLocationOfGoods, "ArrAutLocOfGooHEA65") ++
      buildAndEncodeElem(procedureTypeFlag, "SimProFlaHEA132") ++
      buildAndEncodeElem(Format.dateFormatted(dateTime), "ArrNotDatHEA141")
    }
    </HEAHEA>
}

object Header {

  object Constants {
    val languageCode: LanguageCode     = LanguageCodeEnglish
    val customsSubPlaceLength          = 17
    val arrivalNotificationPlaceLength = 35
  }
}
