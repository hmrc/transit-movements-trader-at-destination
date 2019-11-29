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

import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsString
import play.api.libs.json.Json
import play.api.mvc.PathBindable

class MovementReferenceNumberSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators with EitherValues with OptionValues {

  "a Movement Reference Number" - {

    val pathBindable = implicitly[PathBindable[MovementReferenceNumber]]

    "must bind from a url" in {

      val mrn = MovementReferenceNumber("99IT9876AB88901209")

      val result = pathBindable.bind("mrn", "99IT9876AB88901209")

      result.right.value mustEqual mrn.value
    }

    "must deserialise" in {

      forAll(arbitrary[MovementReferenceNumber]) {
        mrn =>
          JsString(mrn.toString).as[MovementReferenceNumber] mustEqual mrn
      }
    }

    "must serialise" in {

      forAll(arbitrary[MovementReferenceNumber]) {
        mrn =>
          Json.toJson(mrn) mustEqual JsString(mrn.toString)
      }
    }

    "must fail to bind from a string that isn't 18 characters long" in {

      val gen = for {
        digits <- Gen.choose[Int](1, 30).suchThat(_ != 18)
        value  <- Gen.pick(digits, ('A' to 'Z') ++ ('0' to '9'))
      } yield value.mkString

      forAll(gen) {
        invalidMrn =>
          MovementReferenceNumber(invalidMrn) must not be defined
      }
    }

    "must fail to bind from a string that has any characters which aren't upper case or digits" in {

      val gen: Gen[(MovementReferenceNumber, Int, Char)] = for {
        mrn       <- arbitrary[MovementReferenceNumber]
        index     <- Gen.choose(0, 17)
        character <- arbitrary[Char]
      } yield (mrn, index, character)

      forAll(gen) {
        case (mrn, index, character) =>
          val validCharacters = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"

          whenever(!validCharacters.contains(character)) {

            val invalidMrn = mrn.toString.updated(index, character)

            MovementReferenceNumber(invalidMrn) must not be defined
          }
      }
    }

    "must fail to bind from a string that does not have a digit as the first characters" in {

      forAll(arbitrary[MovementReferenceNumber], Gen.alphaUpperChar) {
        (mrn, upperCaseChar) =>
          val invalidMrn = mrn.toString.updated(0, upperCaseChar)

          MovementReferenceNumber(invalidMrn) must not be defined
      }
    }

    "must fail to bind from a string that does not have a digit as the second characters" in {

      forAll(arbitrary[MovementReferenceNumber], Gen.alphaUpperChar) {
        (mrn, upperCaseChar) =>
          val invalidMrn = mrn.toString.updated(1, upperCaseChar)

          MovementReferenceNumber(invalidMrn) must not be defined
      }
    }

    "must fail to bind a string that does not have an upper case character as the third character" in {

      forAll(arbitrary[MovementReferenceNumber], Gen.numChar) {
        (mrn, digit) =>
          val invalidMrn = mrn.toString.updated(2, digit)

          MovementReferenceNumber(invalidMrn) must not be defined
      }
    }

    "must fail to bind a string that does not have an upper case character as the fourth character" in {

      forAll(arbitrary[MovementReferenceNumber], Gen.numChar) {
        (mrn, digit) =>
          val invalidMrn = mrn.toString.updated(3, digit)

          MovementReferenceNumber(invalidMrn) must not be defined
      }
    }

    "must build from a valid MRN" in {

      MovementReferenceNumber("99IT9876AB88901209").value mustEqual MovementReferenceNumber("99", "IT", "9876AB8890120")
      MovementReferenceNumber("18GB0000601001EB15").value mustEqual MovementReferenceNumber("18", "GB", "0000601001EB1")
      MovementReferenceNumber("18GB0000601001EBD1").value mustEqual MovementReferenceNumber("18", "GB", "0000601001EBD")
      MovementReferenceNumber("18IT02110010006A10").value mustEqual MovementReferenceNumber("18", "IT", "02110010006A1")
      MovementReferenceNumber("18IT021100100069F4").value mustEqual MovementReferenceNumber("18", "IT", "021100100069F")
      MovementReferenceNumber("18GB0000601001EBB5").value mustEqual MovementReferenceNumber("18", "GB", "0000601001EBB")
    }

    "must treat .apply and .toString as dual" in {

      forAll(arbitrary[MovementReferenceNumber]) {
        mrn =>
          MovementReferenceNumber(mrn.toString).value mustEqual mrn
      }
    }

    "must fail to bind from inputs with invalid check characters" in {

      val checkDigitPosition = 17

      val gen = for {
        mrn               <- arbitrary[MovementReferenceNumber].map(_.toString)
        invalidCheckDigit <- Gen.alphaChar suchThat (_ != mrn(checkDigitPosition))
      } yield (mrn, invalidCheckDigit)

      forAll(gen) {
        case (mrn, invalidCheckDigit) =>
          val invalidMrn = mrn.toString.updated(checkDigitPosition, invalidCheckDigit)

          MovementReferenceNumber(invalidMrn) must not be defined
      }
    }
  }
}
