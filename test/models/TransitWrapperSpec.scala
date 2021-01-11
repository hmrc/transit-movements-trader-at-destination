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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.xml.Utility.trim

class TransitWrapperSpec extends AnyFreeSpec with Matchers {

  "TransitWrapper" - {
    "must add transit wrapper to an existing xml" in {

      val testNode = <testNode></testNode>
      val result   = TransitWrapper(testNode).toXml

      val expectedResult =
        <transitRequest>{testNode}</transitRequest>

      trim(result).toString() must equal(trim(expectedResult).toString())
    }
  }

}
