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

package models.request

import generators.MessageGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.xml.Utility.trim
import scala.xml.XML.loadString

class CustomsOfficeOfPresentationSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with MessageGenerators {

  "CustomsOfficeOfPresentation" - {

    "must create valid xml" in {
      forAll(arbitrary[CustomsOfficeOfPresentation]) {
        customsOfficeOfPresentation =>
          val expectedResult =
            <CUSOFFPREOFFRES>
              <RefNumRES1>{customsOfficeOfPresentation.presentationOffice}</RefNumRES1>
            </CUSOFFPREOFFRES>

          trim(customsOfficeOfPresentation.toXml) mustBe trim(loadString(expectedResult.toString))
      }
    }
  }

}
