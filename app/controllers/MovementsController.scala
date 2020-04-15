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
import javax.inject.Inject
import models.ArrivalId
import models.Arrivals
import models.MessageSender
import models.MessageType
import models.SubmissionResult
import models.TransitWrapper
import play.api.Logger
import models.MessageState.SubmissionSucceeded
import models.request.ArrivalRequest
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import uk.gov.hmrc.http.BadGatewayException
import uk.gov.hmrc.http.GatewayTimeoutException
import uk.gov.hmrc.http.ServiceUnavailableException
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
  authenticate: AuthenticateActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def createMovement: Action[NodeSeq] = authenticate().async(parse.xml) {
    implicit request =>
      arrivalMovementService.getArrivalMovement(request.eoriNumber, request.body) flatMap {
        case Some(x) => Future.successful(SeeOther(s"/movements/arrivals/${x.arrivalId}"))

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
                                  //TODO: Unpack this for-yield to add recover block
                                  for {
                                    _ <- arrivalMovementRepository.setMessageState(arrival.arrivalId,
                                                                                   arrival.messages.length - 1,
                                                                                   state.transition(SubmissionResult.Success))
                                    _ <- arrivalMovementRepository.setState(arrival.arrivalId, arrival.state.transition(ArrivalSubmitted))
                                  } yield {
                                    Accepted("Message accepted")
                                    // TODO: This needs to be replaced url to arrival movement resource, for which we need an Arrival Movement number
                                      .withHeaders("Location" -> arrival.arrivalId.index.toString)

                                }
                                .recoverWith {
                                  case e: Exception =>
                                    logger.error(s"setState failed with following Exception: ${e.getMessage}")
                                    Future.successful(InternalServerError)
                                }
                          }
                          .recoverWith {
                            case error: Exception =>
                              logger.error(s"Call to EIS failed with the following Exception: ${error.getMessage}")
                              val newState = arrival.state.transition(SubmissionResult.Failure)
                              arrivalMovementRepository.setState(arrival.arrivalId, newState).map {
                                _ =>
                                  BadGateway
                              }
                        }
                    }
                    .recover {
                      case _ => {
                        // TODO: Add logging here so that we can be alerted to these issues.
                        InternalServerError
                      }
                    }
              }
      }

  }

  def updateArrival(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      {
        println("abc")
        arrivalMovementService.makeArrivalNotificationMessage(request.arrival.nextMessageCorrelationId)(request.body) match {
          case None =>
            Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))

          case Some(message) => {
            {
              println("happy path")
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
                                  Accepted.withHeaders("Location" -> request.arrival.arrivalId.index.toString)
                              }
                          }
                    }
                case Failure(e) => {
                  println("A")
                  e.printStackTrace()
                  Future.successful(InternalServerError)
                }
              }
            }.recover {
              case _ => {
                println("B")
                // TODO: Add logging here so that we can be alerted to these issues.
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
}
