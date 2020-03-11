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
import utils.Format

import scala.util.Try
import scala.xml.NodeSeq

class MessageTimestampService {

  def deriveTimestamp(a: NodeSeq): Option[TimeStampedMessageXml] = {

    val dateOfPrep =
      Try {
        LocalDate.parse((a \ "CC007A" \ "DatOfPreMES9").text, Format.dateFormatter)
      }

    val timeOfPrep =
      Try {
        LocalTime.parse((a \ "CC007A" \ "TimOfPreMES10").text, Format.timeFormatter)
      }

    /*
      TODO: Make this applicative composition so that we can report back on ALL fields that are malformed. Also consider switching to using parsers.
     */
    (for {
      d <- dateOfPrep
      t <- timeOfPrep
    } yield {
      TimeStampedMessageXml(d, t, a)
    }).toOption
  }
}
