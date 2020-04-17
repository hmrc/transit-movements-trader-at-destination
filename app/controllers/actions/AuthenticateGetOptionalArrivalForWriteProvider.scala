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

import scala.xml.XML
import javax.inject.Inject
import models.ArrivalId
import models.request.AuthenticatedOptionalArrivalRequest
import models.request.AuthenticatedRequest
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.Locked
import play.api.mvc.Results.BadRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.ActionFunction
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsXml
import play.api.mvc.BodyParser
import play.api.mvc.BodyParsers
import play.api.mvc.ControllerComponents
import play.api.mvc.Request
import play.api.mvc.Result
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.ArrivalMovementService

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class AuthenticatedGetOptionalArrivalForWriteActionProvider @Inject()(
  authenticate: AuthenticateActionProvider,
  arrivalMovementRepository: ArrivalMovementRepository,
  lockRepository: LockRepository,
  ec: ExecutionContext,
  parser: BodyParsers.Default
) {

  def apply(): ActionBuilder[AuthenticatedOptionalArrivalRequest, AnyContent] =
    authenticate() andThen new AuthenticateGetOptionalArrivalForWriteAction(arrivalMovementRepository, lockRepository, ec)
}

class AuthenticateGetOptionalArrivalForWriteAction(
  arrivalMovementRepository: ArrivalMovementRepository,
  lockRepository: LockRepository,
  implicit protected val executionContext: ExecutionContext
) extends ActionFunction[AuthenticatedRequest, AuthenticatedOptionalArrivalRequest] {

  override def invokeBlock[A](request: AuthenticatedRequest[A], block: AuthenticatedOptionalArrivalRequest[A] => Future[Result]): Future[Result] =
    request.body match {
      case body: AnyContentAsXml =>
        ArrivalMovementService.mrnR(XML.loadString(body.xml.toString())) match {
          case None => Future.successful(BadRequest)
          case Some(mrn) => {
            arrivalMovementRepository.get(request.eoriNumber, mrn).flatMap {
              case None => block(AuthenticatedOptionalArrivalRequest(request, None))
              case Some(arrival) =>
                lockRepository.lock(arrival.arrivalId).flatMap {
                  case false => Future.successful(Locked)
                  case true =>
                    block(AuthenticatedOptionalArrivalRequest(request, Some(arrival)))
                      .flatMap {
                        result =>
                          lockRepository.unlock(arrival.arrivalId).map {
                            _ =>
                              result
                          }
                      }
                      .recoverWith {
                        case e: Exception =>
                          lockRepository.unlock(arrival.arrivalId).map {
                            _ =>
                              InternalServerError
                          }
                      }
                }
            }
          }
        }
      case _ => Future.successful(BadRequest)
    }

}
