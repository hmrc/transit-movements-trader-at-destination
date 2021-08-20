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

import cats.data.EitherT
import logging.Logging
import models.Arrival
import models.ArrivalMessageNotification
import models.InboundMessageRequest
import models.MessageSender
import models.MessageType
import models.SubmissionState
import play.api.http.HeaderNames
import play.api.mvc.Headers
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementMessageOrchestratorService @Inject()(
  inboundRequestService: InboundRequestService,
  saveMessageService: SaveMessageService,
  pushPullNotificationService: PushPullNotificationService
)(implicit ec: ExecutionContext)
    extends Logging {

  def saveNCTSMessage(messageSender: MessageSender, requestXml: NodeSeq, headers: Headers)(
    implicit hc: HeaderCarrier): Future[Either[SubmissionState, InboundMessageRequest]] =
    (
      for {
        inboundRequest     <- EitherT(inboundRequestService.makeInboundRequest(messageSender.arrivalId, requestXml, messageSender))
        saveInboundRequest <- EitherT(saveMessageService.saveInboundMessage(inboundRequest, messageSender))
        sendPushNotification <- EitherT.right[SubmissionState](
          pushPullNotificationService
            .sendPushNotification(requestXml, inboundRequest.arrival, inboundRequest.inboundMessageResponse.messageType, headers))
      } yield inboundRequest
    ).value

}
