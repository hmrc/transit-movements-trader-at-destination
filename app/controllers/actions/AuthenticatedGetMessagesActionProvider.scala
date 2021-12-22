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

package controllers.actions

import logging.Logging
import models.ArrivalId
import models.MessageType
import models.request.ArrivalsMessagesRequest
import models.request.AuthenticatedRequest
import play.api.mvc.ActionRefiner
import play.api.mvc.Result
import play.api.mvc.Results.BadRequest
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import repositories.ArrivalMovementRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class AuthenticatedGetMessagesActionProvider @Inject()(
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext) {

  def apply(arrivalId: ArrivalId, messageTypes: List[MessageType]): ActionRefiner[AuthenticatedRequest, ArrivalsMessagesRequest] =
    new AuthenticatedGetMessagesAction(arrivalId, repository, messageTypes)

}

private[actions] class AuthenticatedGetMessagesAction(
  arrivalId: ArrivalId,
  repository: ArrivalMovementRepository,
  messageTypes: List[MessageType]
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, ArrivalsMessagesRequest]
    with Logging {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, ArrivalsMessagesRequest[A]]] =
    ChannelUtil.getChannel(request) match {
      case None =>
        logger.warn(s"Missing channel header for request id ${request.headers.get("http_x_request_id")}")
        Future.successful(Left(BadRequest("Missing channel header or incorrect value specified in channel header")))
      case Some(channel) =>
        repository
          .getMessagesOfType(arrivalId, channel, messageTypes)
          .map {
            case Some(arrivalMessages) if request.hasMatchingEnrolmentId(arrivalMessages) && !arrivalMessages.messages.isEmpty =>
              Right(ArrivalsMessagesRequest(request, arrivalId, channel, arrivalMessages.messages))
            case Some(arrivalMessages) if request.hasMatchingEnrolmentId(arrivalMessages) =>
              logger.warn(s"No messages of types $messageTypes were found for the given movement")
              Left(NotFound)
            case Some(_) =>
              logger.warn("Attempt to retrieve an arrival for another EORI")
              Left(NotFound)
            case None =>
              Left(NotFound)
          }
          .recover {
            case e =>
              logger.error(s"Failed with the following error: $e")
              Left(InternalServerError)
          }
    }
}
