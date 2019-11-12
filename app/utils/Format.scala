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

package utils

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

object Format {

  val dateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
  def dateFormatted(date: LocalDate) = date.format(dateFormatter)
  def dateFormatted(date: LocalDateTime) = date.format(dateFormatter)

  val timeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HHMM")
  def timeFormatted(date: LocalDateTime) = date.format(timeFormatter)
}
