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

package utils

import org.scalatest.StreamlinedXmlEquality
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class XMLTransformerSpec extends AnyFreeSpec with Matchers with StreamlinedXmlEquality {

  "XMLTransformer" - {
    "must add a node of an xml document" in {
      val xml        = <main><test>data</test></main>
      val updatedXml = XMLTransformer.addXmlNode("test", "test1", "newData", xml)
      updatedXml.toString() mustEqual <main><test>data</test><test1>newData</test1></main>.toString()
    }

    /*    "must remove same xml if node is missing" in {
      val xml        = <main><test>data</test></main>
      val updatedXml = XMLTransformer.updateXmlNode("test1", "newData", xml)
      updatedXml.toString() mustEqual xml.toString()
    }*/
  }
}
