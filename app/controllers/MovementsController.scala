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
import javax.inject.Inject
import models.MessageType
import models.SubmissionResult
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import services.DatabaseService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementsController @Inject()(
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementService,
  databaseService: DatabaseService,
  authenticate: AuthenticateActionProvider,
  messageConnector: MessageConnector
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def createMovement: Action[NodeSeq] = authenticate().async(parse.xml) {
    implicit request =>
      arrivalMovementService.makeArrivalMovement(request.eoriNumber)(request.body) match {
        case None =>
          Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))

        case Some(x) =>
          x.flatMap {
              arrival =>
                arrivalMovementRepository.insert(arrival) flatMap {
                  _ =>
                    messageConnector
                      .post(request.body.toString, MessageType.ArrivalNotification, OffsetDateTime.now)
                      .flatMap {
                        _ =>
                          val newState = arrival.state.transition(SubmissionResult.Success)
                          arrivalMovementRepository.setState(arrival.arrivalId, newState).map {
                            _ =>
                              Accepted("Message accepted")
                              // TODO: This needs to be replaced url to arrival movement resource, for which we need an Arrival Movement number
                                .withHeaders("Location" -> arrival.arrivalId.index.toString)
                          }
                      }
                      .recoverWith {
                        case _ =>
                          val newState = arrival.state.transition(SubmissionResult.Failure)
                          arrivalMovementRepository.setState(arrival.arrivalId, newState).map {
                            _ =>
                              InternalServerError
                          }
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

  def getMovements: Action[AnyContent] = authenticate().async {
    implicit request =>
      arrivalMovementRepository
        .fetchAllMovements(request.eoriNumber)
        .map {
          arrivalMovements =>
            // TODO this needs further iteration
            val messages = arrivalMovements.map(_.messages.head)
            Ok(Json.toJson(messages))
        }
        .recover {
          case e =>
            InternalServerError(s"Failed with the following error: $e")
        }
  }
}
