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
import javax.inject.Inject
import models.ArrivalId
import models.MessageType
import models.MovementMessageWithState
import models.SubmissionResult
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class MessagesController @Inject()(
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementService,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def post(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request =>
      if (MessageType.isMessageTypeSupported(request.body)) {
        MessageType.getMessageType(request.body) match {
          case Some(messageType) =>
            arrivalMovementService.makeMessage(request.arrival.nextMessageCorrelationId, messageType)(request.body) match {
              case None =>
                Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
              case Some(message) =>
                arrivalMovementRepository
                  .addNewMessage(request.arrival.arrivalId, message)
                  .flatMap {
                    case Failure(t) => Future.failed(t)
                    case Success(_) =>
                      messageConnector
                      // TODO: Fix this casting
                        .post(request.arrival.arrivalId, request.arrival.messages.head.asInstanceOf[MovementMessageWithState], OffsetDateTime.now)
                        .flatMap {
                          _ =>
                            arrivalMovementRepository
                              .setMessageState(request.arrival.arrivalId, request.arrival.messages.length, message.state.transition(SubmissionResult.Success))
                              .flatMap {
                                case Failure(t) =>
                                  Future.failed(t)
                                case Success(_) =>
                                  Future.successful(Accepted.withHeaders("Location" -> routes.MessagesController.post(request.arrival.arrivalId).url))
                              }
                              .recover {
                                case _ =>
                                  InternalServerError
                              }
                        }
                        .recoverWith {
                          case error =>
                            logger.error(s"Call to EIS failed with the following exception:", error)
                            arrivalMovementRepository
                              .setMessageState(request.arrival.arrivalId, request.arrival.messages.length, message.state.transition(SubmissionResult.Failure))
                              .flatMap {
                                case Failure(t) =>
                                  Future.failed(t)
                                case Success(_) =>
                                  Future.successful(BadGateway)
                              }
                              .recover {
                                case _ =>
                                  BadGateway
                              }
                        }
                  }
                  .recover {
                    case _ => {
                      InternalServerError
                    }
                  }
            }
          case None =>
            Future.successful(NotImplemented)
        }
      } else {
        Future.successful(NotImplemented)
      }
  }
}
