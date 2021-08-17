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

import javax.inject.Inject
import logging.Logging
import models.ArrivalId
import models.request.ArrivalWithoutMessagesRequest
import models.request.AuthenticatedRequest
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import play.api.mvc.Results.BadRequest
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class GetArrivalWithoutMessagesActionProvider @Inject() (
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext) {

  def apply(arrivalId: ArrivalId): ActionRefiner[Request, ArrivalWithoutMessagesRequest] =
    new GetArrivalWithoutMessagesAction(arrivalId, repository)
}

private[actions] class GetArrivalWithoutMessagesAction(
  arrivalId: ArrivalId,
  repository: ArrivalMovementRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[Request, ArrivalWithoutMessagesRequest] {

  override protected def refine[A](request: Request[A]): Future[Either[Result, ArrivalWithoutMessagesRequest[A]]] =
    repository.getWithoutMessages(arrivalId).map {
      case Some(arrivalWithoutMessages) =>
        Right(ArrivalWithoutMessagesRequest(request, arrivalWithoutMessages, arrivalWithoutMessages.channel))
      case None =>
        Left(NotFound)
    }
}

private[actions] class AuthenticatedGetArrivalWithoutMessagesActionProvider @Inject() (
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext) {

  def apply(arrivalId: ArrivalId): ActionRefiner[AuthenticatedRequest, ArrivalWithoutMessagesRequest] =
    new AuthenticatedGetArrivalWithoutMessagesAction(arrivalId, repository)
}

private[actions] class AuthenticatedGetArrivalWithoutMessagesAction(
  arrivalId: ArrivalId,
  repository: ArrivalMovementRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, ArrivalWithoutMessagesRequest]
    with Logging {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, ArrivalWithoutMessagesRequest[A]]] =
    ChannelUtil.getChannel(request) match {
      case None =>
        logger.warn(s"Missing channel header for request id ${request.headers.get("http_x_request_id")}")
        Future.successful(Left(BadRequest("Missing channel header or incorrect value specified in channel header")))
      case Some(channel) =>
        repository
          .getWithoutMessages(arrivalId, channel)
          .map {
            case Some(arrivalWithoutMessages) if arrivalWithoutMessages.eoriNumber == request.eoriNumber =>
              Right(ArrivalWithoutMessagesRequest(request.request, arrivalWithoutMessages, channel))
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
