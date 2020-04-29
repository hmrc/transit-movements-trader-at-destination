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

import controllers.actions.GetArrivalForWriteActionProvider
import javax.inject.Inject
import models.ArrivalStatus
import models.GoodsReleasedResponse
import models.MessageResponse
import models.MessageSender
import models.MessageType
import models.UnloadingPermissionResponse
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementService
import services.XmlValidationService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class MessageResponseController @Inject()(cc: ControllerComponents,
                                          arrivalMovementService: ArrivalMovementService,
                                          getArrival: GetArrivalForWriteActionProvider,
                                          arrivalMovementRepository: ArrivalMovementRepository,
                                          xmlValidationService: XmlValidationService)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def post(messageSender: MessageSender): Action[NodeSeq] = getArrival(messageSender.arrivalId)(parse.xml).async {
    implicit request =>
      val xml: NodeSeq = request.request.body

      val messageResponse: Option[MessageResponse] = request.headers.get("X-Message-Type") match {
        case Some(MessageType.GoodsReleased.code)       => Some(GoodsReleasedResponse)
        case Some(MessageType.UnloadingPermission.code) => Some(UnloadingPermissionResponse)
        case invalidResponse =>
          logger.error(s"Received the following invalid response for X-Message-Type: $invalidResponse")
          None
      }

      messageResponse match {
        case Some(response) =>
          xmlValidationService.validate(xml.toString, response.xsdFile) match {
            case Success(_) =>
              arrivalMovementService.makeMessage(messageSender.messageCorrelationId, response.messageType)(xml) match {
                case Some(message) =>
                  val newState: ArrivalStatus = request.arrival.status.transition(response.messageReceived)
                  arrivalMovementRepository.addResponseMessage(request.arrival.arrivalId, message, newState).map {
                    case Success(_) => Ok
                    case Failure(e) =>
                      logger.error(s"Failure to add message to movement. Exception: ${e.getMessage}")
                      InternalServerError
                  }
                case None =>
                  logger.error(s"Failure to parse message")
                  Future.successful(BadRequest)
              }
            case Failure(e) =>
              logger.error(s"Failure to validate against XSD. Exception: ${e.getMessage}")
              Future.successful(BadRequest)
          }
        case None =>
          Future.successful(BadRequest)
      }

  }
}
