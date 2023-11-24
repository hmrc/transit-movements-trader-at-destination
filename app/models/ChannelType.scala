/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.json.Format
import play.api.libs.json.JsError
import play.api.libs.json.JsResult
import play.api.libs.json.JsString
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsValue

sealed trait ChannelType

object ChannelType extends Enumerable.Implicits {
  case object web extends ChannelType
  case object api extends ChannelType

  val values: Seq[ChannelType] = Seq(web, api)

  implicit val enumerable: Enumerable[ChannelType] =
    Enumerable(
      values.map(
        v => v.toString -> v
      ): _*
    )

  implicit val formats: Format[ChannelType] = new Format[ChannelType] {

    override def reads(json: JsValue): JsResult[ChannelType] = json match {
      case JsString("web") => JsSuccess(web)
      case JsString("api") => JsSuccess(api)
      case _               => JsError("error.invalid")
    }

    override def writes(o: ChannelType): JsValue = JsString(o.toString)
  }
}
