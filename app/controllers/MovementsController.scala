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
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import controllers.actions.AuthenticatedGetOptionalArrivalForWriteActionProvider
import javax.inject.Inject
import models.MessageReceived.ArrivalSubmitted
import models.Arrival
import models.ArrivalId
import models.MessageReceived
import models.MessageSender
import models.MessageState
import models.MessageType
import models.SubmissionResult
import models.TransitWrapper
import models.request.ArrivalRequest
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
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
  authenticatedOptionalArrival: AuthenticatedGetOptionalArrivalForWriteActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def createMovement: Action[NodeSeq] = authenticatedOptionalArrival().async(parse.xml) {
    implicit request =>
      request.arrival match {
        case Some(arrival) => update_internal(arrival, request.body)
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
                                      .withHeaders("Location" -> routes.GetArrivalController.getArrival(arrival.arrivalId).url)
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

  private def update_internal(arrival: Arrival, body: NodeSeq)(implicit hc: HeaderCarrier) =
    arrivalMovementService.makeArrivalNotificationMessage(arrival.nextMessageCorrelationId)(body) match {
      case None => Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
      case Some(message) => {
        arrivalMovementRepository.addNewMessage(arrival.arrivalId, message).flatMap {
          case Failure(_) =>
            Future.successful(InternalServerError)
          case Success(()) =>
            messageConnector
              .post(TransitWrapper(body),
                    MessageType.ArrivalNotification,
                    OffsetDateTime.now,
                    MessageSender(arrival.arrivalId, arrival.nextMessageCorrelationId))
              .flatMap {
                _ =>
                  arrivalMovementRepository
                    .setMessageState(arrival.arrivalId, arrival.messages.length, MessageState.SubmissionSucceeded)
                    .flatMap {
                      _ =>
                        arrivalMovementRepository.setState(arrival.arrivalId, arrival.state.transition(MessageReceived.ArrivalSubmitted)).map {
                          _ =>
                            Accepted("Message accepted")
                              .withHeaders("Location" -> routes.GetArrivalController.getArrival(arrival.arrivalId).url)
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
                    .setMessageState(arrival.arrivalId, arrival.messages.length, message.state.transition(SubmissionResult.Failure))
                    .map {
                      _ =>
                        BadGateway
                    }
              }

        }
      }
    }

  def updateArrival(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      update_internal(request.arrival, request.body)
  }
}
