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

import play.api.mvc.PathBindable

import scala.util.Try

final case class MessageSender(arrivalId: ArrivalId, version: Int) {
  override val toString = s"MDTP-${arrivalId.index}-$version"
}

object MessageSender {

  private val pattern = "MDTP-(\\d+)-(\\d+)".r.anchored

  def apply(value: String): Option[MessageSender] =
    value match {
      case pattern(id, version) =>
        (for {
          id <- Try(id.toInt)
          v  <- Try(version.toInt)
        } yield {
          MessageSender(ArrivalId(id), v)
        }).toOption
      case _ => None
    }

  implicit lazy val pathBindable: PathBindable[MessageSender] = new PathBindable[MessageSender] {
    override def bind(key: String, value: String): Either[String, MessageSender] =
      apply(value) match {
        case Some(messageSender) => Right(messageSender)
        case None                => Left("Invalid message sender")
      }

    override def unbind(key: String, value: MessageSender): String =
      value.toString
  }
}
