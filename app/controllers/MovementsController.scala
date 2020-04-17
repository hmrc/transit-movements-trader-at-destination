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
import controllers.actions.LockActionProvider
import javax.inject.Inject
import models.MessageReceived.ArrivalSubmitted
import models.ArrivalId
import models.Arrivals
import models.MessageReceived
import models.MessageSender
import models.MessageState
import models.MessageType
import models.SubmissionResult
import models.TransitWrapper
import models.request.ArrivalRequest
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.DefaultActionBuilder
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class MovementsController @Inject()(
  defaultActionBuilder: DefaultActionBuilder,
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementService,
  authenticate: AuthenticateActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def createMovement: Action[NodeSeq] = authenticate().async(parse.xml) {
    implicit request =>
      arrivalMovementService.getArrivalMovement(request.eoriNumber, request.body) flatMap {
        case Some(arrival) =>
          //TODO: Should this just be a redirect to the update endpoint? Will that work?
          arrivalMovementService.makeArrivalNotificationMessage(arrival.nextMessageCorrelationId)(request.body) match {
            case None => Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
            case Some(message) => {
              arrivalMovementRepository.addMessage(arrival.arrivalId, message).flatMap {
                case Failure(_) => Future.successful(InternalServerError)
                case Success(()) => {
                  messageConnector
                    .post(TransitWrapper(request.body),
                          MessageType.ArrivalNotification,
                          OffsetDateTime.now,
                          MessageSender(arrival.arrivalId, arrival.nextMessageCorrelationId))
                    .flatMap {
                      _ =>
                        arrivalMovementRepository.setMessageState(arrival.arrivalId, arrival.messages.length, MessageState.SubmissionSucceeded).flatMap {
                          _ =>
                            arrivalMovementRepository.setState(arrival.arrivalId, arrival.state.transition(MessageReceived.ArrivalSubmitted)).map {
                              _ =>
                                Accepted("Message accepted")
                                  .withHeaders("Location" -> routes.MovementsController.get(arrival.arrivalId).url)
                            }
                        }
                    }
                    .recover {
                      case _ => InternalServerError
                    }
                }
              }
            }
          }
        case None =>
          arrivalMovementService.makeArrivalMovement(request.eoriNumber)(request.body) match {
            case None =>
              Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))

            case Some(x) =>
              x.flatMap {
                  arrival =>
                    arrivalMovementRepository.insert(arrival) flatMap {
                      _ =>
                        messageConnector
                          .post(
                            TransitWrapper(request.body),
                            MessageType.ArrivalNotification,
                            OffsetDateTime.now,
                            MessageSender(arrival.arrivalId, arrival.messages.head.messageCorrelationId)
                          )
                          .flatMap {
                            _ =>
                              arrival.messages.last.getState() match {
                                case Some(state) =>
                                  for {
                                    _ <- arrivalMovementRepository.setMessageState(arrival.arrivalId,
                                                                                   arrival.messages.length - 1,
                                                                                   state.transition(SubmissionResult.Success))
                                    _ <- arrivalMovementRepository.setState(arrival.arrivalId, arrival.state.transition(ArrivalSubmitted))
                                  } yield {
                                    Accepted("Message accepted")
                                      .withHeaders("Location" -> routes.MovementsController.get(arrival.arrivalId).url)
                                  }
                                case None =>
                                  Future.successful(InternalServerError)
                              }
                          }
                          .recoverWith {
                            case error =>
                              logger.error(s"Call to EIS failed with the following Exception: ${error.getMessage}")

                              arrival.messages.last.getState() match {
                                case Some(state) =>
                                  arrivalMovementRepository
                                    .setMessageState(arrival.arrivalId, arrival.messages.length - 1, state.transition(SubmissionResult.Failure))
                                    .map {
                                      _ =>
                                        BadGateway
                                    }
                                case None =>
                                  Future.successful(BadGateway)
                              }
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

  def updateArrival(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      {
        arrivalMovementService.makeArrivalNotificationMessage(request.arrival.nextMessageCorrelationId)(request.body) match {
          case None =>
            Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))

          case Some(message) => {
            {
              arrivalMovementRepository.addMessage(arrivalId, message).flatMap {
                case Success(()) =>
                  messageConnector
                    .post(TransitWrapper(request.body),
                          MessageType.ArrivalNotification,
                          OffsetDateTime.now,
                          MessageSender(arrivalId, request.arrival.nextMessageCorrelationId))
                    .flatMap {
                      _ =>
                        arrivalMovementRepository
                          .setMessageState(arrivalId, request.arrival.messages.length, message.state.transition(SubmissionResult.Success))
                          .flatMap {
                            _ =>
                              arrivalMovementRepository.setState(arrivalId, request.arrival.state.transition(ArrivalSubmitted)).map {
                                _ =>
                                  Accepted.withHeaders("Location" -> routes.MovementsController.get(request.arrival.arrivalId).url)
                              }
                          }
                          .recover {
                            case _ => {
                              InternalServerError
                            }
                          }
                    }
                    .recoverWith {
                      case error =>
                        logger.error(s"Call to EIS failed with the following Exception: ${error.getMessage}")
                        arrivalMovementRepository
                          .setMessageState(arrivalId, request.arrival.messages.length, message.state.transition(SubmissionResult.Failure))
                          .map {
                            _ =>
                              BadGateway
                          }
                    }
                case Failure(e) => {
                  Future.successful(InternalServerError)
                }
              }
            }.recover {
              case _ => {
                InternalServerError
              }
            }
          }
        }
      }

  }

  def getArrivals: Action[AnyContent] = authenticate().async {
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

  def get(arrivalId: ArrivalId): Action[AnyContent] = defaultActionBuilder(_ => NotImplemented)
}
