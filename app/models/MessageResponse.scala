/*
 * Copyright 2023 HM Revenue & Customs
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

import logging.Logging
import models.XSDFile._

sealed trait MessageResponse {
  val messageReceived: MessageReceivedEvent
  val messageType: MessageType
  val auditType: String
}

object MessageResponse extends Logging {

  val inboundMessages = Seq(
    GoodsReleasedResponse,
    ArrivalRejectedResponse,
    UnloadingPermissionResponse,
    UnloadingRemarksRejectedResponse,
    XMLSubmissionNegativeAcknowledgementResponse
  )

  val outboundMessages = Seq(
    UnloadingRemarksResponse
  )

  def getMessageResponseFromCode(code: String): Either[SubmissionState, MessageResponse] =
    code match {
      case MessageType.GoodsReleased.rootNode             => Right(GoodsReleasedResponse)
      case MessageType.ArrivalRejection.rootNode          => Right(ArrivalRejectedResponse)
      case MessageType.UnloadingPermission.rootNode       => Right(UnloadingPermissionResponse)
      case MessageType.UnloadingRemarks.rootNode          => Right(UnloadingRemarksResponse)
      case MessageType.UnloadingRemarksRejection.rootNode => Right(UnloadingRemarksRejectedResponse)
      case MessageType.XMLSubmissionNegativeAcknowledgement.rootNode =>
        logger.error(s"Received the message ${MessageType.XMLSubmissionNegativeAcknowledgement.code}")
        Right(XMLSubmissionNegativeAcknowledgementResponse)
      case _ => Left(InvalidArrivalRootNodeError(s"[MessageResponse][getMessageResponseFromCode] Unrecognised code: $code"))
    }
}

sealed trait OutboundMessageResponse extends MessageResponse

sealed trait InboundMessageResponse extends MessageResponse {
  val xsdFile: XSDFile
}

case object UnloadingRemarksResponse extends OutboundMessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.UnloadingRemarksSubmitted
  override val messageType: MessageType              = MessageType.UnloadingRemarks
  override val auditType: String                     = "UnloadingRemarksSubmitted"
}

case object GoodsReleasedResponse extends InboundMessageResponse {
  override val messageReceived          = MessageReceivedEvent.GoodsReleased
  override val messageType: MessageType = MessageType.GoodsReleased
  override val xsdFile: XSDFile         = GoodsReleasedXSD
  override val auditType: String        = "GoodsReleased"
}

case object ArrivalRejectedResponse extends InboundMessageResponse {
  override val messageReceived          = MessageReceivedEvent.ArrivalRejected
  override val messageType: MessageType = MessageType.ArrivalRejection
  override val xsdFile: XSDFile         = ArrivalRejectedXSD
  override val auditType: String        = "ArrivalNotificationRejected"
}

case object UnloadingPermissionResponse extends InboundMessageResponse {
  override val messageReceived          = MessageReceivedEvent.UnloadingPermission
  override val messageType: MessageType = MessageType.UnloadingPermission
  override val xsdFile: XSDFile         = UnloadingPermissionXSD
  override val auditType: String        = "UnloadingPermissionReceived"
}

case object UnloadingRemarksRejectedResponse extends InboundMessageResponse {
  override val messageReceived          = MessageReceivedEvent.UnloadingRemarksRejected
  override val messageType: MessageType = MessageType.UnloadingRemarksRejection
  override val xsdFile: XSDFile         = UnloadingRemarksRejectedXSD
  override val auditType: String        = "UnloadingPermissionRejected"
}

case object XMLSubmissionNegativeAcknowledgementResponse extends InboundMessageResponse {
  override val messageReceived          = MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
  override val messageType: MessageType = MessageType.XMLSubmissionNegativeAcknowledgement
  override val xsdFile: XSDFile         = InvalidXmlXSD
  override val auditType: String        = "XMLSubmissionNegativeAcknowledgement"
}
