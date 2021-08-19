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

import models.InboundMessageError
import models.InboundMessageResponse
import models.MessageResponse
import models.OutboundMessageError
import models.OutboundMessageResponse
import models.SubmissionState

object MessageValidationService {

  def validateInboundMessage(messageResponse: MessageResponse): Either[SubmissionState, InboundMessageResponse] =
    messageResponse match {
      case response: InboundMessageResponse => Right(response)
      case response: OutboundMessageResponse =>
        Left(InboundMessageError(s"[MessageValidationService][validateInboundMessage] ${response.messageType.code} is not an inbound message type"))
    }

  def validateOutboundMessage(messageResponse: MessageResponse): Either[SubmissionState, OutboundMessageResponse] =
    messageResponse match {
      case response: OutboundMessageResponse => Right(response)
      case response: InboundMessageResponse =>
        Left(OutboundMessageError(s"[MessageValidationService][validateOutboundMessage] ${response.messageType.code} is not an outbound message type"))
    }

}
