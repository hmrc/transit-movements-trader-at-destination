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

case class Header(
  movementReferenceNumber: String,
  customsSubPlace: Option[String] = None,
  arrivalNotificationPlace: String,
  arrivalAgreedLocationOfGoods: Option[String] = None,
  simplifiedProcedureFlag: String
)

object Header {
  object Constants {
    val languageCode: LanguageCode = LanguageCodeEnglish
    val customsSubPlaceLength = 17
    val arrivalNotificationPlaceLength = 35
    val simplifiedProcedureFlag0 = "0"
    val simplifiedProcedureFlag1 = "1"
  }
}
