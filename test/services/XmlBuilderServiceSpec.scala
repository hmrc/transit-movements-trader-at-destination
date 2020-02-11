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

package services

import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues

import scala.xml.Elem
import scala.xml.Utility.trim

class XmlBuilderServiceSpec extends FreeSpec with MustMatchers with OptionValues {

  val xmlBuilderService = new XmlBuilderService

  val testXml: Elem = <C007A></C007A>

  "XmlBuilderService" - {

    "must add transit wrapper to an existing xml" in {

      val result = xmlBuilderService.buildXmlWithTransitWrapper(testXml).right.toOption.value
      val expectedResult =
        <transitRequest xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                        xsi:noNamespaceSchemaLocation="../../schema/request/request.xsd">{testXml}</transitRequest>

      trim(result) mustBe trim(expectedResult)
    }

  }
}
