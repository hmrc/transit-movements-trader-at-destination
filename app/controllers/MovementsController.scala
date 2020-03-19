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

import controllers.actions.IdentifierAction
import javax.inject.Inject
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import reactivemongo.api.commands.WriteResult
import repositories.ArrivalMovementRepository
import repositories.FailedSavingArrivalMovement
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
  identify: IdentifierAction
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def createMovement: Action[NodeSeq] = identify.async(parse.xml) {
    implicit request =>
      arrivalMovementService.makeArrivalMovement(request.eoriNumber)(request.body) match {
        case None =>
          Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))

        case Some(x) =>
          x.flatMap {
              arrival =>
                arrivalMovementRepository.insert(arrival) map {
                  _ =>
                    Accepted("Message accepted")
                    // TODO: This needs to be replaced url to arrival movement resource, for which we need an Arrival Movement number
                      .withHeaders("Location" -> arrival.arrivalId.index.toString)
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

  def getMovements: Action[AnyContent] = identify.async {
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
