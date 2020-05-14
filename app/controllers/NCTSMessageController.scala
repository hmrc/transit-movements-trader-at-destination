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
import models.ArrivalRejectedResponse
import models.GoodsReleasedResponse
import models.MessageResponse
import models.MessageSender
import models.MessageType
import models.SubmissionProcessingResult
import models.UnloadingPermissionResponse
import models.SubmissionProcessingResult._
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services.SaveMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class NCTSMessageController @Inject()(cc: ControllerComponents, getArrival: GetArrivalForWriteActionProvider, saveMessageService: SaveMessageService)(
  implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post(messageSender: MessageSender): Action[NodeSeq] = getArrival(messageSender.arrivalId)(parse.xml).async {
    implicit request =>
      val xml: NodeSeq = request.request.body

      val messageResponse: Option[MessageResponse] = request.headers.get("X-Message-Type") match {
        case Some(MessageType.GoodsReleased.code)       => Some(GoodsReleasedResponse)
        case Some(MessageType.ArrivalRejection.code)    => Some(ArrivalRejectedResponse)
        case Some(MessageType.UnloadingPermission.code) => Some(UnloadingPermissionResponse)
        case invalidResponse =>
          Logger.warn(s"Received the following invalid response for X-Message-Type: $invalidResponse")
          None
      }

      messageResponse match {
        case Some(response) =>
          val newState         = request.arrival.status.transition(response.messageReceived)
          val processingResult = saveMessageService.validateXmlAndSaveMessage(xml, messageSender, response, newState)
          processingResult map {
            case SubmissionSuccess => Ok
            case SubmissionFailureInternal =>
              val message = "Internal Submission Failure " + processingResult
              Logger.warn(message)
              InternalServerError(message)
            case SubmissionFailureExternal =>
              val message = "External Submission Failure " + processingResult
              Logger.warn(message)
              BadRequest(message)
          }
        case None =>
          val message = "No response from downstream NCTS";
          Logger.warn(message)
          Future.successful(BadRequest(message))
      }

  }
}
