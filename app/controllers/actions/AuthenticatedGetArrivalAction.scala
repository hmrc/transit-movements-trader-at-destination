/*
 * Copyright 2022 HM Revenue & Customs
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
import models.request.ArrivalRequest
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

private[actions] class AuthenticatedGetArrivalActionProvider @Inject() (
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext) {

  def apply(arrivalId: ArrivalId): ActionRefiner[AuthenticatedRequest, ArrivalRequest] =
    new AuthenticatedGetArrivalAction(arrivalId, repository)
}

private[actions] class AuthenticatedGetArrivalAction(
  arrivalId: ArrivalId,
  repository: ArrivalMovementRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, ArrivalRequest]
    with Logging {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, ArrivalRequest[A]]] =
    ChannelUtil.getChannel(request) match {
      case None =>
        logger.warn(s"Missing channel header for request id ${request.headers.get("http_x_request_id")}")
        Future.successful(Left(BadRequest("Missing channel header or incorrect value specified in channel header")))
      case Some(channel) =>
        repository
          .get(arrivalId, channel)
          .map {
            case Some(arrival) if request.hasMatchingEnrolmentId(arrival) =>
              Right(ArrivalRequest(request, arrival, channel))
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
