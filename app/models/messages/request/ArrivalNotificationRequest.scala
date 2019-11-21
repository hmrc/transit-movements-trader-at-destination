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

package models.messages.request

import models.messages.MessageCode

import scala.collection.immutable.ListMap


trait RequestModel
trait RequestModelError


object FailedToConvert                extends RequestModelError
object FailedToCreateXml              extends RequestModelError
object FailedToValidateXml            extends RequestModelError
object FailedToWriteToMongo           extends RequestModelError

case class ArrivalNotificationRequest(
                                       meta: Meta,
                                       header: Header,
                                       traderDestination: TraderDestination,
                                       customsOfficeOfPresentation: CustomsOfficeOfPresentation
                                     ) extends RequestConstants with RequestModel {

  val messageCode: MessageCode = MessageCode("GB007A")
  val syntaxIdentifier: String = "UNOC"
  val rootKey: String = "CC007A"
  val nameSpace: Map[String, String] = ListMap(
//    "xmlns:xsi" -> "http://www.w3.org/2001/XMLSchema-instance",
//    "xmlns" -> "http://ncts.dgtaxud.ec/CC007A",
//    "xmlns:complex_ncts" -> "http://ncts.dgtaxud.ec/complex_ncts",
//    "xsi:schemaLocation" -> "http://ncts.dgtaxud.ec/CC007A"
  )
}

object ArrivalNotificationRequest {
  val messageCode: MessageCode = MessageCode("GB007A")
}
