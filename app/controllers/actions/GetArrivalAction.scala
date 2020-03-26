/*
 * Copyright 2020 HM Revenue & Customs
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
import models.request.ArrivalId
import models.request.ArrivalRequest
import models.request.AuthenticatedRequest
import play.api.Logger
import play.api.mvc.Results.NotFound
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class GetArrivalActionProvider @Inject()(
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext) {

  def apply(arrivalId: ArrivalId): ActionRefiner[Request, ArrivalRequest] =
    new GetArrivalAction(arrivalId, repository)
}

class GetArrivalAction(
  arrivalId: ArrivalId,
  repository: ArrivalMovementRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[Request, ArrivalRequest] {

  override protected def refine[A](request: Request[A]): Future[Either[Result, ArrivalRequest[A]]] =
    repository.get(arrivalId).map {
      case Some(arrival) =>
        Right(ArrivalRequest(request, arrival))
      case None =>
        Left(NotFound)
    }
}

class AuthenticatedGetArrivalActionProvider @Inject()(
  repository: ArrivalMovementRepository
)(implicit ec: ExecutionContext) {

  def apply(arrivalId: ArrivalId): ActionRefiner[AuthenticatedRequest, ArrivalRequest] =
    new AuthenticatedGetArrivalAction(arrivalId, repository)
}

class AuthenticatedGetArrivalAction(
  arrivalId: ArrivalId,
  repository: ArrivalMovementRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, ArrivalRequest] {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, ArrivalRequest[A]]] =
    repository.get(arrivalId).map {
      case Some(arrival) if arrival.eoriNumber == request.eoriNumber =>
        Right(ArrivalRequest(request.request, arrival)) //TODO: We could pass on the overall IdentifierRequest if we had an AuthenticatedArrivalRequest
      case Some(_) =>
        Logger.warn("Attempt to retrieve an arrival for another EORI")
        Left(NotFound)
      case _ =>
        Left(NotFound)
    }
}
