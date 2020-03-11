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

package services

import java.time.LocalDate
import java.time.LocalTime

import models.TimeStampedMessageXml

import scala.concurrent.Future
import scala.util.matching.Regex
import scala.xml.Atom
import scala.xml.NodeSeq

class MessageTimestampService {

  def deriveTimestamp(a: NodeSeq): Option[TimeStampedMessageXml] = {
    val dateOfPrepRegex: Regex = """(\d{4})(\d{2})(\d{2})""".r.anchored
    val timeOfPrepRegex: Regex = """(\d{2})(\d{2})""".r.anchored

    val dateOfPrep: Option[LocalDate] =
      (a \ "CC007A" \ "DatOfPreMES9").text match {
        case dateOfPrepRegex(year, month, date) =>
          Some(LocalDate.of(year.toInt, month.toInt, date.toInt)) // we shouldn't do unsafe casting like this but is it fine in this case because of the Regex?
        case _ => None
      }

    val timeOfPrep: Option[LocalTime] =
      (a \ "CC007A" \ "TimOfPreMES10").text match {
        case timeOfPrepRegex(hours, minutes) =>
          Some(LocalTime.of(hours.toInt, minutes.toInt)) // TODO: we shouldn't do unsafe casting like this but is it fine in this case because of the Regex?
        case _ => None
      }

    /*
      TODO: Make this applicative composition so that we can report back on ALL fields that are malformed. Also consider switching to using parsers.
     */
    for {
      d <- dateOfPrep
      t <- timeOfPrep
    } yield TimeStampedMessageXml(d, t, a)
  }
}
