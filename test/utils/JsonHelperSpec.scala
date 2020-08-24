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

import org.scalatest.EitherValues
import org.scalatest.TryValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.JsObject
import play.api.libs.json.Json

import scala.util.Try

class JsonHelperSpec extends AnyFreeSpec with Matchers with TryValues {

  "XmlToJsonService" - {

    "must convert xml to json" in {
      val xml = "<xml><test1>one</test1><test1>two</test1></xml>"

      val expectedResult: JsObject = Json.obj("xml" -> Json.obj("test1" -> Json.arr("one", "two")))
      val result: Try[JsObject]    = JsonHelper.convertXmlToJson(xml)
      result.success.value.toString mustBe expectedResult.toString()
    }

    "must return Failure on failing to convert xml to json" in {
      val invalidXml           = "<xml><test1>one</test1><test1></xml>"
      val expectedErrorMessage = "Mismatched test1 and xml at 35 [character 36 line 1]"

      val result: Try[JsObject] = JsonHelper.convertXmlToJson(invalidXml)
      result.failure.exception.getMessage mustBe expectedErrorMessage
    }
  }
}
