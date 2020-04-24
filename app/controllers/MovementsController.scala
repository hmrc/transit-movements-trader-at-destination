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

import java.time.OffsetDateTime

import connectors.MessageConnector
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import controllers.actions.AuthenticatedGetOptionalArrivalForWriteActionProvider
import javax.inject.Inject
import models.MessageReceived.ArrivalSubmitted
import models.MessageState.SubmissionFailed
import models.MessageState.SubmissionSucceeded
import models.Arrival
import models.ArrivalId
import models.Arrivals
import models.MessageId
import models.MessageReceived
import models.MessageState
import models.MovementMessageWithState
import models.SubmissionResult
import models.request.ArrivalRequest
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.DefaultActionBuilder
import play.api.mvc.Result
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import services.SubmitMessageService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class MovementsController @Inject()(
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementService,
  submitMessageService: SubmitMessageService,
  authenticate: AuthenticateActionProvider,
  authenticatedOptionalArrival: AuthenticatedGetOptionalArrivalForWriteActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  defaultActionBuilder: DefaultActionBuilder,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def post: Action[NodeSeq] = authenticatedOptionalArrival().async(parse.xml) {
    implicit request =>
      request.arrival match {
        case Some(arrival) =>
          arrivalMovementService
            .makeArrivalNotificationMessage(arrival.nextMessageCorrelationId)(request.body)
            .map {
              message =>
                submitMessageService
                  .submit(arrival.arrivalId, new MessageId(arrival.messages.length - 1), message)
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
                    arrivalMovementRepository.insert(arrival) flatMap {
                      _ =>
                        messageConnector
                        // TODO: Fix this casting
                          .post(arrival.arrivalId, arrival.messages.head.asInstanceOf[MovementMessageWithState], OffsetDateTime.now)
                          .flatMap {
                            _ =>
                              for {
                                _ <- arrivalMovementRepository
                                  .setMessageState(arrival.arrivalId, 0, SubmissionSucceeded) // TODO: use the message's state transition here and don't hard code the index of the message
                                _ <- arrivalMovementRepository.setState(arrival.arrivalId, arrival.state.transition(ArrivalSubmitted))
                              } yield {
                                Accepted("Message accepted")
                                  .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)
                              }
                          }
                          .recoverWith {
                            case error =>
                              logger.error(s"Call to EIS failed with the following Exception: ${error.getMessage}")
                              arrivalMovementRepository
                                .setMessageState(arrival.arrivalId, arrival.messages.length - 1, SubmissionFailed)
                                .map(_ => BadGateway)
                          }
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
              .submit(arrivalId, new MessageId(request.arrival.messages.length - 1), message)
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
}
