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
import models.MessageResponse.inboundMessages
import models.MessageResponse.outboundMessages
import models.InboundMessageError
import models.OutboundMessageError
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MessageValidationServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  "MessageValidationService" - {

    "validateInboundMessage" - {

      "must return MessageResponse when an InboundMessage type" in {

        forAll(Gen.oneOf(inboundMessages)) {
          inboundMessage =>
            val result = MessageValidationService.validateInboundMessage(inboundMessage).value

            result mustBe inboundMessage
        }
      }

      "must return InboundMessageError when message type is that of an Outbound message" in {

        forAll(Gen.oneOf(outboundMessages)) {
          outboundMessage =>
            val result = MessageValidationService.validateInboundMessage(outboundMessage).left.value

            result mustBe an[InboundMessageError]
        }
      }

    }

    "validateOutboundMessage" - {

      "must return MessageResponse when an OutboundMessage type" in {

        forAll(Gen.oneOf(outboundMessages)) {
          outboundMessage =>
            val result = MessageValidationService.validateOutboundMessage(outboundMessage).value

            result mustBe outboundMessage
        }
      }

      "must return OutboundMessageError when message type is that of an InboundMessage" in {

        forAll(Gen.oneOf(inboundMessages)) {
          inboundMessage =>
            val result = MessageValidationService.validateOutboundMessage(inboundMessage).left.value

            result mustBe an[OutboundMessageError]
        }
      }
    }
  }
}
