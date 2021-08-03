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
import cats.implicits.catsStdInstancesForFuture
import models.ArrivalId
import models.CannotFindRootNodeError
import models.InboundMessageRequest
import models.MessageResponse
import models.StatusTransition
import models.SubmissionState

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class InboundRequestService @Inject()(lockService: LockService, getArrivalService: GetArrivalService)(implicit ec: ExecutionContext) {

  def inboundRequest(arrivalId: ArrivalId, xml: NodeSeq): Future[Either[SubmissionState, InboundMessageRequest]] =
    (
      for {
        lock                   <- EitherT(lockService.lock(arrivalId))
        arrival                <- EitherT(getArrivalService.getArrivalById(arrivalId))
        headNode               <- EitherT.fromOption(xml.headOption, CannotFindRootNodeError(s"[InboundRequest][inboundRequest] Could not find root node"))
        messageResponse        <- EitherT.fromEither(MessageResponse.getMessageResponseFromCode(headNode.label, arrival.channel))
        nextStatus             <- EitherT.fromEither(StatusTransition.transition(arrival.status, messageResponse.messageReceived))
        inboundMessageResponse <- EitherT.fromEither(MessageValidationService.validateInboundMessage(messageResponse))
        unlock                 <- EitherT(lockService.unlock(arrivalId))
      } yield InboundMessageRequest(arrival, nextStatus, inboundMessageResponse)
    ).value
}

//auditService.auditNCTSMessages(arrival.channel, inboundRequest.inboundMessageResponse, message)
