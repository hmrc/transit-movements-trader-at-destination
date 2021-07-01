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

package utils

import models.ArrivalId
import models.ParseError.MesSenMES3Failure
import org.scalatest.StreamlinedXmlEquality
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.xml.NodeSeq

class XMLTransformerSpec extends AnyFreeSpec with Matchers with StreamlinedXmlEquality {

  "XMLTransformer" - {

    "addXmlNode" - {

      "must" - {

        "add a node" in {
          val xml        = <main><test>data</test></main>
          val updatedXml = XMLTransformer.addXmlNode("test", "test1", "newData", xml)
          updatedXml.toString() mustEqual <main><test>data</test><test1>newData</test1></main>.toString()
        }

        "replace existing node with new one" in {
          val xml        = <main><test>data</test><test1>testData</test1></main>
          val updatedXml = XMLTransformer.addXmlNode("test", "test1", "newData", xml)
          updatedXml.toString() mustEqual <main><test>data</test><test1>newData</test1></main>.toString()
        }

        "return the same xml if the node does not exist" in {
          val xml        = <main><test2>newData</test2></main>
          val updatedXml = XMLTransformer.addXmlNode("test", "test1", "newData", xml)
          updatedXml.toString() mustEqual xml.toString()
        }

      }

    }

    "updateMesSenMES3" - {

      "return failure if SynVerNumMES2 node doesn't exist" in {

        val movement = <node></node>

        XMLTransformer.updateMesSenMES3(ArrivalId(1), 1)(movement).left.get mustBe MesSenMES3Failure(
          s"Failed to set MesSenMES3 because SynVerNumMES2 is missing")
      }

      "add MesSenMES3 node if SynVerNumMES2 exists" in {

        val movement: NodeSeq = <SynVerNumMES2>test</SynVerNumMES2>

        val updatedMovement: NodeSeq = <SynVerNumMES2>test</SynVerNumMES2><MesSenMES3>MDTP-ARR-00000000000000000000001-01</MesSenMES3>

        XMLTransformer.updateMesSenMES3(ArrivalId(1), 1)(movement).right.get mustEqual updatedMovement
      }

      "replace MesSenMES3 node if SynVerNumMES2 and MesSenMES3 exists" in {

        val movement: NodeSeq = <SynVerNumMES2>test</SynVerNumMES2><MesSenMES3>replaced</MesSenMES3>

        val updatedMovement: NodeSeq = <SynVerNumMES2>test</SynVerNumMES2><MesSenMES3>MDTP-ARR-00000000000000000000001-01</MesSenMES3>

        XMLTransformer.updateMesSenMES3(ArrivalId(1), 1)(movement).right.get mustEqual updatedMovement
      }

    }

  }

}
