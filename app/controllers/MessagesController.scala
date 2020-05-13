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

import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import javax.inject.Inject
import models.ArrivalId
import models.ArrivalStatus
import models.MessageId
import models.MessageStatus.SubmissionFailed
import models.MessageType
import models.SubmissionProcessingResult
import models.request.ArrivalRequest
import models.response.ResponseArrival
import models.response.ResponseMovementMessage
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.ArrivalMovementService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessagesController @Inject()(
  cc: ControllerComponents,
  arrivalMovementService: ArrivalMovementService,
  submitMessageService: SubmitMessageService,
  authenticateForRead: AuthenticatedGetArrivalForReadActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val logger = Logger(getClass)

  def post(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      MessageType.getMessageType(request.body) match {
        case Some(MessageType.UnloadingRemarks) =>
          arrivalMovementService
            .makeMovementMessageWithStatus(request.arrival.nextMessageCorrelationId, MessageType.UnloadingRemarks)(request.body)
            .map {
              message =>
                val messageId = new MessageId(request.arrival.messages.length)
                submitMessageService
                  .submitMessage(arrivalId, messageId, message, ArrivalStatus.UnloadingRemarksSubmitted)
                  .map {
                    case SubmissionProcessingResult.SubmissionSuccess =>
                      Accepted("Message accepted")
                        .withHeaders("Location" -> routes.MessagesController.getMessage(request.arrival.arrivalId, messageId).url)

                    case SubmissionProcessingResult.SubmissionFailureInternal =>
                      InternalServerError

                    case SubmissionProcessingResult.SubmissionFailureExternal =>
                      BadGateway
                  }
            }
            .getOrElse {
              logger.error("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5")
              Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
            }
        case _ =>
          Future.successful(NotImplemented)
      }
  }

  def getMessage(arrivalId: ArrivalId, messageId: MessageId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      if (request.arrival.messages.isDefinedAt(messageId.index) && request.arrival.messages(messageId.index).optStatus != Some(SubmissionFailed))
        Ok(Json.toJsObject(ResponseMovementMessage.build(arrivalId, messageId, request.arrival.messages(messageId.index))))
      else NotFound
  }

  def getMessages(arrivalId: ArrivalId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      Ok(Json.toJsObject(ResponseArrival.build(request.arrival)))
  }
}
