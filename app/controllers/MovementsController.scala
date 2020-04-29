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

package controllers

import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import controllers.actions.AuthenticatedGetOptionalArrivalForWriteActionProvider
import javax.inject.Inject
import models.ArrivalId
import models.ArrivalStatus
import models.Arrivals
import models.MessageId
import models.SubmissionResult
import models.request.ArrivalRequest
import models.response.ResponseMovementMessage
import models.MessageStatus.SubmissionFailed
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.DefaultActionBuilder
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementsController @Inject()(
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementService,
  submitMessageService: SubmitMessageService,
  authenticate: AuthenticateActionProvider,
  authenticateForRead: AuthenticatedGetArrivalForReadActionProvider,
  authenticatedOptionalArrival: AuthenticatedGetOptionalArrivalForWriteActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  defaultActionBuilder: DefaultActionBuilder
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post: Action[NodeSeq] = authenticatedOptionalArrival().async(parse.xml) {
    implicit request =>
      request.arrival match {
        case Some(arrival) =>
          arrivalMovementService
            .makeArrivalNotificationMessage(arrival.nextMessageCorrelationId)(request.body)
            .map {
              message =>
                submitMessageService
                  .submitMessage(arrival.arrivalId, new MessageId(arrival.messages.length - 1), message, ArrivalStatus.ArrivalSubmitted)
                  .map {
                    case SubmissionResult.Success =>
                      Accepted("Message accepted")
                        .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)

                    case SubmissionResult.FailureInternal =>
                      InternalServerError

                    case SubmissionResult.FailureExternal =>
                      BadGateway
                  }
            }
            .getOrElse(Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5")))

        case None =>
          arrivalMovementService.makeArrivalMovement(request.eoriNumber)(request.body) match {
            case None =>
              Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
            case Some(arrivalFuture) =>
              arrivalFuture
                .flatMap {
                  arrival =>
                    submitMessageService.submitArrival(arrival) map {
                      case SubmissionResult.Success =>
                        Accepted("Message accepted")
                          .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)
                      case SubmissionResult.FailureExternal =>
                        BadGateway
                      case SubmissionResult.FailureInternal =>
                        InternalServerError
                    }
                }
                .recover {
                  case _ => {
                    InternalServerError
                  }
                }
          }
      }
  }

  def putArrival(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      arrivalMovementService
        .makeArrivalNotificationMessage(request.arrival.nextMessageCorrelationId)(request.body)
        .map {
          message =>
            submitMessageService
              .submitMessage(arrivalId, new MessageId(request.arrival.messages.length - 1), message, ArrivalStatus.ArrivalSubmitted)
              .map {
                case SubmissionResult.Success =>
                  Accepted("Message accepted")
                    .withHeaders("Location" -> routes.MovementsController.getArrival(request.arrival.arrivalId).url)

                case SubmissionResult.FailureInternal =>
                  InternalServerError

                case SubmissionResult.FailureExternal =>
                  BadGateway
              }
        }
        .getOrElse(Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5")))

  }

  def getArrival(arrivalId: ArrivalId): Action[AnyContent] = defaultActionBuilder(_ => NotImplemented)

  def getArrivals(): Action[AnyContent] = authenticate().async {
    implicit request =>
      arrivalMovementRepository
        .fetchAllArrivals(request.eoriNumber)
        .map {
          allArrivals =>
            Ok(Json.toJsObject(Arrivals(allArrivals)))
        }
        .recover {
          case e =>
            InternalServerError(s"Failed with the following error: $e")
        }
  }

  def getMessage(arrivalId: ArrivalId, messageId: MessageId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      if (request.arrival.messages.isDefinedAt(messageId.index) && request.arrival.messages(messageId.index).optStatus != Some(SubmissionFailed))
        Ok(Json.toJson(ResponseMovementMessage.build(arrivalId, messageId, request.arrival.messages(messageId.index))))
      else NotFound
  }
}
