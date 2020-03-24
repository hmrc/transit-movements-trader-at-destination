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
import play.api.mvc.ActionFunction
import play.api.mvc._
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ArrivalRetrievalActionProviderImpl @Inject()(repository: ArrivalMovementRepository, ec: ExecutionContext, parser: BodyParsers.Default)
    extends ArrivalRetrievalActionProvider {

  override def apply(arrivalId: ArrivalId): ActionBuilder[ArrivalRequest, AnyContent] with ActionFunction[Request, ArrivalRequest] =
    new ArrivalRetrievalAction(arrivalId, ec, repository, parser)
}

trait ArrivalRetrievalActionProvider {
  def apply(arrivalId: ArrivalId): ActionBuilder[ArrivalRequest, AnyContent] with ActionFunction[Request, ArrivalRequest]
}

class ArrivalRetrievalAction(
  arrivalId: ArrivalId,
  implicit protected val executionContext: ExecutionContext,
  repository: ArrivalMovementRepository,
  val parser: BodyParsers.Default
) extends ActionBuilder[ArrivalRequest, AnyContent]
    with ActionFunction[Request, ArrivalRequest] {

  override def invokeBlock[A](request: Request[A], block: ArrivalRequest[A] => Future[Result]): Future[Result] =
    repository.get(arrivalId).flatMap {
      arrival =>
        block(ArrivalRequest(request, arrival))
    }
}
