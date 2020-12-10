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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers.running

class MessageTranslationSpec extends AnyFreeSpec with Matchers {

  val appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder().configure("message-translation-file" -> "TestMessageTranslation.json")

  ".translate" - {

    "must replace all occurrences of strings in the message translation file" in {

      val app = appBuilder.build()

      running(app) {
        val service = app.injector.instanceOf[MessageTranslation]
        val input   = "field1, field1, field2, field3"
        val result  = service.translate(input)

        result mustEqual "Description 1, Description 1, Description 2, field3"
      }
    }
  }
}
