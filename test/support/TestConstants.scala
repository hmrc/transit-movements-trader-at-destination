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

package support

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import models.{Trader, TraderWithEori, TraderWithoutEori}
import models.messages.NormalNotification

object TestConstants {

  val date: LocalDate = LocalDate.now
  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
  val dateFormatted: String = date.format(dateTimeFormatter)

  lazy val traderWithEori = TraderWithEori("GB163910077000", None, None, None, None, None)
  lazy val traderWithoutEori = TraderWithoutEori("name", "street", "postcode", "city", "country code")

  def normalArrivalNotification(trader: Trader) = NormalNotification(
    movementReferenceNumber = "19IT02110010007827",
    notificationPlace = "DOVER",
    notificationDate = date,
    customsSubPlace =  None,
    trader = trader,
    presentationOffice = "GB000060",
    enRouteEvents = Seq.empty)
}
