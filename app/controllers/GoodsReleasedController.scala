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

import config.AppConfig
import controllers.actions.GetArrivalForWriteActionProvider
import javax.inject.Inject
import models.MessageReceived
import models.MessageSender
import models.XSDFile.GoodsReleasedXSD
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

class GoodsReleasedController @Inject()(cc: ControllerComponents,
                                        appConfig: AppConfig,
                                        arrivalMovementService: ArrivalMovementService,
                                        getArrival: GetArrivalForWriteActionProvider,
                                        arrivalMovementRepository: ArrivalMovementRepository,
                                        xmlValidationService: XmlValidationService)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def post(messageSender: MessageSender): Action[NodeSeq] = getArrival(messageSender.arrivalId)(parse.xml).async {
    implicit request =>
      val xml: NodeSeq = request.request.body

      xmlValidationService.validate(xml.toString, GoodsReleasedXSD) match {
        case Success(_) =>
          arrivalMovementService.makeGoodsReleasedMessage()(xml) match {
            case Some(message) =>
              val newState = request.arrival.state.transition(MessageReceived.GoodsReleased)
              arrivalMovementRepository.addMessage(request.arrival.arrivalId, message, newState).map {
                case Success(_) => Ok
                case Failure(_) => InternalServerError
              }
            case None =>
              Future.successful(InternalServerError)
          }
        case Failure(e) =>
          logger.error(e.getMessage)
          Future.successful(BadRequest)
      }
  }
}
