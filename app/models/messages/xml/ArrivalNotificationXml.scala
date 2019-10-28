/*
 * Copyright 2019 HM Revenue & Customs
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

package models.messages.xml

sealed trait MetaConstants {
  val syntaxIdentifier: String = "UNOC"
  val syntaxVersionNumber: String = "3"
  val messageRecipient: String = "NCTS"

}
case class Meta(
                 mesSenMES3: String,
                 senIdeCodQuaMES4: Option[String],
                 recIdeCodQuaMES7: Option[String],
                 datOfPreMES9: String,
                 timOfPreMES10: String,
                 intConRefMES11: String,
                 recRefMES12: Option[String],
                 recRefQuaMES13: Option[String],
                 appRefMES14: Option[String],
                 priMES15: Option[String],
                 ackReqMES16: Option[String],
                 comAgrIdMES17: Option[String],
                 tesIndMES18: Option[String],
                 mesIdeMES19: String,
                 mesTypMES20: String,
                 comAccRefMES21: Option[String],
                 mesSeqNumMES22: Option[String],
                 firAndLasTraMES23: Option[String]) extends MetaConstants


case class Header(
                   docNumHEA5: String,
                   cusSubPlaHEA66: Option[String],
                   arrNotPlaHEA60: String,
                   arrNotPlaHEA60LNG: Option[String],
                   arrAgrLocCodHEA62: Option[String],
                   arrAgrLocOfGooHEA63: Option[String],
                   arrAgrLocOfGooHEA63LNG: Option[String],
                   arrAutLocOfGooHEA65: Option[String],
                   simProFlaHEA132: Option[String],
                   arrNotDatHEA141: String,
                   diaLanIndAtDesHEA255: Option[String])

case class TraderDestination(
                              namTRD7String: Option[String],
                              strAndNumTRD22: Option[String],
                              posCodTRD23: Option[String],
                              citTRD24: Option[String],
                              couTRD25: Option[String],
                              tintrd59: Option[String]
                            ) {
  val nadlngrd: String = "EN"
}

case class CustomsOfficeOfPresentation(
                                        refNumRES1: String
                                      )

case class ArrivalNotificationRootNode(
                                        key: String = "CC007A",
                                        nameSpace: Map[String, String] = Map(
                                          "xmlns=" -> "http://ncts.dgtaxud.ec/CC007A",
                                          "xmlns:xsi=" -> "http://www.w3.org/2001/XMLSchema-instance",
                                          "xmlns:complex_ncts=" -> "http://ncts.dgtaxud.ec/complex_ncts",
                                          "xsi:schemaLocation=" -> "http://ncts.dgtaxud.ec/CC007A"))

case class ArrivalNotificationXml(
                                   rootNode: ArrivalNotificationRootNode = ArrivalNotificationRootNode(),
                                   meta: Meta,
                                   header: Header,
                                   traderDestination: TraderDestination,
                                   customsOfficeOfPresentation: CustomsOfficeOfPresentation
                                 )

object ArrivalNotificationXml {
//  val rootNode: String = "CC007A"
  val messageCode: String = "GB007A"
  val syntaxIdentifier: String = "UNOC"
//  val syntaxVersionNumber: String = "3"
 // val messageRecipient: String = "NCTS"
  val applicationReference: String = "NCTS"
  val testIndicator: String = "0"
  val messageIdentification: String = "1"
  val languageCode: Option[String] = Some("EN")
//  val nameSpace: Map[String, String] = Map(
//    "xmlns=" -> "http://ncts.dgtaxud.ec/CC007A",
//    "xmlns:xsi=" -> "http://www.w3.org/2001/XMLSchema-instance",
//    "xmlns:complex_ncts=" -> "http://ncts.dgtaxud.ec/complex_ncts",
//    "xsi:schemaLocation=" -> "http://ncts.dgtaxud.ec/CC007A")
}
