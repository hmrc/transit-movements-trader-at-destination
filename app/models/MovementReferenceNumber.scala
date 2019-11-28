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

import MovementReferenceNumber._
import play.api.libs.json.JsString
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.mvc.PathBindable

import scala.math.pow

final case class MovementReferenceNumber(
  year: String,
  countryCode: String,
  serial: String
) {

  override def toString: String = s"$year$countryCode$serial$checkCharacter"

  val checkCharacter: String = {

    val input = s"$year$countryCode$serial"

    val remainder = input.zipWithIndex.map {
      case (character, index) =>
        characterWeights(character) * pow(2, index).toInt
    }.sum % 11

    (remainder % 10).toString
  }
}

object MovementReferenceNumber {

  private val mrnFormat = """^(\d{2})([A-Z]{2})([A-Z0-9]{13})(\d)$""".r

  def apply(input: String): Option[MovementReferenceNumber] = input match {
    case mrnFormat(year, countryCode, serial, checkCharacter) =>
      val mrn = MovementReferenceNumber(year, countryCode, serial)

      if (mrn.checkCharacter == checkCharacter) {
        Some(mrn)
      } else {
        None
      }

    case _ =>
      None
  }

  implicit lazy val reads: Reads[MovementReferenceNumber] = {

    import play.api.libs.json._

    __.read[String].map(MovementReferenceNumber.apply).flatMap {
      case Some(mrn) => Reads(_ => JsSuccess(mrn))
      case None      => Reads(_ => JsError("Invalid Movement Reference Number"))
    }
  }

  implicit lazy val writes: Writes[MovementReferenceNumber] = Writes {
    mrn =>
      JsString(mrn.toString)
  }

  implicit def pathBindable: PathBindable[MovementReferenceNumber] = new PathBindable[MovementReferenceNumber] {

    override def bind(key: String, value: String): Either[String, MovementReferenceNumber] =
      MovementReferenceNumber.apply(value).toRight("Invalid Movement Reference Number")

    override def unbind(key: String, value: MovementReferenceNumber): String =
      value.toString
  }

  private val characterWeights = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'A' -> 10,
    'B' -> 12,
    'C' -> 13,
    'D' -> 14,
    'E' -> 15,
    'F' -> 16,
    'G' -> 17,
    'H' -> 18,
    'I' -> 19,
    'J' -> 20,
    'K' -> 21,
    'L' -> 23,
    'M' -> 24,
    'N' -> 25,
    'O' -> 26,
    'P' -> 27,
    'Q' -> 28,
    'R' -> 29,
    'S' -> 30,
    'T' -> 31,
    'U' -> 32,
    'V' -> 34,
    'W' -> 35,
    'X' -> 36,
    'Y' -> 37,
    'Z' -> 38
  )
}
