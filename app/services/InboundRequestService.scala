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

import akka.actor.TypedActor.dispatcher
import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture
import models.ArrivalId
import models.ArrivalStatus
import models.CannotFindRootNodeError
import models.InboundMessageResponse
import models.MessageResponse
import models.RequestError
import models.StatusTransition

import javax.inject.Inject
import scala.concurrent.Future
import scala.xml.NodeSeq

case class InboundMessageRequest(nextStatus: ArrivalStatus, inboundMessageResponse: InboundMessageResponse)

class InboundRequestService @Inject()(lockService: LockService, getArrivalService: GetArrivalService) {

  def inboundRequest(arrivalId: ArrivalId, xml: NodeSeq): EitherT[Future, RequestError, InboundMessageRequest] =
    for {
      _                      <- EitherT(lockService.lock(arrivalId))
      arrival                <- EitherT(getArrivalService.getArrivalById(arrivalId))
      headNode               <- EitherT.fromOption[Future](xml.headOption, CannotFindRootNodeError(s"[InboundRequest][inboundRequest] Could not find root node"))
      messageResponse        <- EitherT.fromEither[Future](MessageResponse.getMessageResponseFromCode(headNode.label, arrival.channel))
      nextStatus             <- EitherT.fromEither[Future](StatusTransition.transition(arrival.status, messageResponse.messageReceived))
      inboundMessageResponse <- EitherT.fromEither[Future](MessageValidationService.validateInboundMessage(messageResponse))
      _                      <- EitherT(lockService.unlock(arrivalId))
    } yield InboundMessageRequest(nextStatus, inboundMessageResponse)
}
