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

package services

import base.SpecBase
import generators.ModelGenerators
import models.MessageSenderError
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.xml.NodeSeq

class MessageSenderServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  "MessageSenderService" - {
    "validateMessageSenderIsEmpty" - {

      "should return unit if there is no message sender" in {

        val nodeSeq: NodeSeq = <OtherXml>OtherValue</OtherXml>

        val result = MessageSenderService.validateMessageSenderIsEmpty(nodeSeq)

        result.value mustBe (())
      }

      "should return MessageSenderError if there is a message sender" in {
        val nodeSeq: NodeSeq = <MesSenMES3>MessageSender</MesSenMES3>

        val result = MessageSenderService.validateMessageSenderIsEmpty(nodeSeq)

        result.left.value mustBe an[MessageSenderError]
      }
    }
  }
}
