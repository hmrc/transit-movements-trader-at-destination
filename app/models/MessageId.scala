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

package models

import play.api.libs.json.Format
import play.api.libs.json.JsError
import play.api.libs.json.JsNumber
import play.api.libs.json.JsResult
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsValue
import play.api.mvc.PathBindable

import scala.util.Try

final class MessageId(val index: Int) extends AnyVal

object MessageId {

  implicit val pathBindableMessageId: PathBindable[MessageId] = new PathBindable[MessageId] {
    override def bind(key: String, value: String): Either[String, MessageId] =
      implicitly[PathBindable[Int]]
        .bind(key, value)
        .fold(
          Left(_), {
            case x if x > 0 => Right(new MessageId(x - 1))
            case x          => Left(s"Invalid MessageId. The MessageId must be > 0, instead got $x")
          }
        )

    override def unbind(key: String, value: MessageId): String =
      (value.index + 1).toString
  }

}
