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
import models.MessageType
import models.SubmissionProcessingResult._
import models.MessageStatus.SubmissionFailed
import models.request.ArrivalRequest
import models.response.ResponseArrivalWithMessages
import models.response.ResponseMovementMessage
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.ArrivalMovementMessageService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessagesController @Inject()(
  cc: ControllerComponents,
  arrivalMovementService: ArrivalMovementMessageService,
  submitMessageService: SubmitMessageService,
  authenticateForRead: AuthenticatedGetArrivalForReadActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      arrivalMovementService
        .makeMovementMessageWithStatus(request.arrival.nextMessageCorrelationId, MessageType.UnloadingRemarks)(request.body) match {
        case Right(message) =>
          submitMessageService
            .submitMessage(arrivalId, request.arrival.nextMessageId, message, ArrivalStatus.UnloadingRemarksSubmitted)
            .map {
              case SubmissionProcessingResult.SubmissionSuccess =>
                Accepted("Message accepted")
                  .withHeaders("Location" -> routes.MessagesController.getMessage(request.arrival.arrivalId, request.arrival.nextMessageId).url)

              case SubmissionProcessingResult.SubmissionFailureInternal =>
                InternalServerError

              case SubmissionProcessingResult.SubmissionFailureExternal =>
                BadGateway
      MessageType.getMessageType(request.body) match {
        case Some(MessageType.UnloadingRemarks) =>
          arrivalMovementService
            .makeMovementMessageWithStatus(request.arrival.nextMessageCorrelationId, MessageType.UnloadingRemarks)(request.body)
            .map {
              message =>
                submitMessageService
                  .submitMessage(arrivalId, request.arrival.nextMessageId, message, ArrivalStatus.UnloadingRemarksSubmitted)
                  .map {
                    case SubmissionFailureInternal => InternalServerError
                    case SubmissionFailureExternal => BadGateway
                    case SubmissionFailureRejected => BadRequest("Failed schema validation")
                    case SubmissionSuccess =>
                      Accepted("Message accepted")
                        .withHeaders("Location" -> routes.MessagesController.getMessage(request.arrival.arrivalId, request.arrival.nextMessageId).url)
                  }
            }
        case Left(error) =>
          Logger.warn(s"Failed to create MovementMessageWithStatus with error: $error")
          Future.successful(BadRequest(s"Failed to create MovementMessageWithStatus with error: $error"))
      }
  }

  def getMessage(arrivalId: ArrivalId, messageId: MessageId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      val messages = request.arrival.messages.toList

      if (messages.isDefinedAt(messageId.index) && messages(messageId.index).optStatus != Some(SubmissionFailed))
        Ok(Json.toJsObject(ResponseMovementMessage.build(arrivalId, messageId, messages(messageId.index))))
      else NotFound
  }

  def getMessages(arrivalId: ArrivalId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      Ok(Json.toJsObject(ResponseArrivalWithMessages.build(request.arrival)))
  }
}
