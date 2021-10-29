/*
 * Copyright 2021 HM Revenue & Customs
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

package models

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import play.api.libs.json.Reads
import play.api.libs.json.__

import java.time.LocalDateTime

case class MessageMetaData(messageType: MessageType, dateTime: LocalDateTime) extends MessageTypeWithTime

object MessageMetaData extends MongoDateTimeFormats {

  implicit val reads: Reads[MessageMetaData] =
    (
      (__ \ "messageType").read[MessageType] and
        (__ \ "dateTime").read[LocalDateTime]
    )(MessageMetaData.apply _)

  implicit val writes: OWrites[MessageMetaData] = OWrites {
    messageMetaData =>
      Json.obj(
        "messageType" -> messageMetaData.messageType,
        "dateTime"    -> messageMetaData.dateTime.toString
      )
  }
}
