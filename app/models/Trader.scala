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

package models

import play.api.libs.json._

sealed trait Trader

object Trader {

  implicit lazy val reads: Reads[Trader] = {
    implicit class ReadsWithContravariantOr[A](a: Reads[A]) {

      def or[B >: A](b: Reads[B]): Reads[B] = a.map[B](identity).orElse(b)
    }

    implicit def convertToSupertype[A, B >: A](a: Reads[A]): Reads[B] = a.map(identity)

    TraderWithEori.format or TraderWithoutEori.format
  }

  implicit lazy val writes: Writes[Trader] = Writes {
    case t: TraderWithEori    => Json.toJson(t)(TraderWithEori.format)
    case t: TraderWithoutEori => Json.toJson(t)(TraderWithoutEori.format)
  }
}

final case class TraderWithEori(eori: String,
                                name: Option[String],
                                streetAndNumber: Option[String],
                                postCode: Option[String],
                                city: Option[String],
                                countryCode: Option[String])
    extends Trader

object TraderWithEori {

  implicit lazy val format: Format[TraderWithEori] = Json.format[TraderWithEori]
}

final case class TraderWithoutEori(name: String, streetAndNumber: String, postCode: String, city: String, countryCode: String) extends Trader

object TraderWithoutEori {

  implicit lazy val format: Format[TraderWithoutEori] = Json.format[TraderWithoutEori]
}
