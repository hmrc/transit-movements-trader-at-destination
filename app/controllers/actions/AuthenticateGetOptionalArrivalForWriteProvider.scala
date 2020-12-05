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
import logging.Logging
import models.request.AuthenticatedOptionalArrivalRequest
import models.request.AuthenticatedRequest
import play.api.mvc._
import play.api.mvc.Results.BadRequest
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.Locked
import repositories.ArrivalMovementRepository
import repositories.LockRepository
import services.XmlMessageParser

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

trait AuthenticatedGetOptionalArrivalForWriteActionProvider {
  def apply(): ActionBuilder[AuthenticatedOptionalArrivalRequest, AnyContent]
}

class AuthenticatedGetOptionalArrivalForWriteActionProviderImpl @Inject()(
  authenticate: AuthenticateActionProvider,
  arrivalMovementRepository: ArrivalMovementRepository,
  lockRepository: LockRepository,
  ec: ExecutionContext
) extends AuthenticatedGetOptionalArrivalForWriteActionProvider {

  def apply(): ActionBuilder[AuthenticatedOptionalArrivalRequest, AnyContent] =
    authenticate() andThen new AuthenticateGetOptionalArrivalForWriteAction(arrivalMovementRepository, lockRepository, ec)
}

class AuthenticateGetOptionalArrivalForWriteAction(
  arrivalMovementRepository: ArrivalMovementRepository,
  lockRepository: LockRepository,
  implicit protected val executionContext: ExecutionContext
) extends ActionFunction[AuthenticatedRequest, AuthenticatedOptionalArrivalRequest]
    with Logging {

  override def invokeBlock[A](request: AuthenticatedRequest[A], block: AuthenticatedOptionalArrivalRequest[A] => Future[Result]): Future[Result] =
    request.body match {
      case body: NodeSeq =>
        XmlMessageParser.mrnR(body) match {
          case Left(error) =>
            logger.error(s"Failed to retrieve MovementReferenceNumber with error: $error")
            Future.successful(BadRequest(s"Failed to retrieve MovementReferenceNumber with error: $error"))

          case Right(mrn) => {
            arrivalMovementRepository.get(request.eoriNumber, mrn).flatMap {
              case None => block(AuthenticatedOptionalArrivalRequest(request, None, request.eoriNumber))
              case Some(arrival) =>
                lockRepository.lock(arrival.arrivalId).flatMap {
                  case false => Future.successful(Locked)
                  case true =>
                    block(AuthenticatedOptionalArrivalRequest(request, Some(arrival), request.eoriNumber))
                      .flatMap {
                        result =>
                          lockRepository.unlock(arrival.arrivalId).map {
                            _ =>
                              result
                          }
                      }
                      .recoverWith {
                        case _: Exception =>
                          lockRepository.unlock(arrival.arrivalId).map {
                            _ =>
                              InternalServerError
                          }
                      }
                }
            }
          }
        }
      case invalidBody =>
        logger.warn(s"Invalid request body: ${invalidBody.getClass}")
        Future.successful(BadRequest(s"Invalid request body: ${invalidBody.getClass}"))
    }
}
