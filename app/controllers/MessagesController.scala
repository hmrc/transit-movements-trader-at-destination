/*
 * Copyright 2021 HM Revenue & Customs
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

import audit.AuditService
import audit.AuditType
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import controllers.actions.MessageTransformerInterface
import controllers.actions._
import logging.Logging
import metrics.MetricsService
import metrics.Monitors
import models.MessageStatus.SubmissionFailed
import models.ArrivalId
import models.MessageId
import models.MessageType
import models.SubmissionProcessingResult._
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

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessagesController @Inject()(
  cc: ControllerComponents,
  arrivalMovementService: ArrivalMovementMessageService,
  submitMessageService: SubmitMessageService,
  authenticateForRead: AuthenticatedGetArrivalForReadActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  validateMessageSenderNode: ValidateMessageSenderNodeFilter,
  auditService: AuditService,
  validateTransitionState: MessageTransformerInterface,
  validateOutboundMessage: ValidateOutboundMessageAction,
  metricsService: MetricsService
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  private val movementSummaryLogger: Logger =
    Logger(s"application.${this.getClass.getCanonicalName}.movementSummary")

  def post(arrivalId: ArrivalId): Action[NodeSeq] =
    (authenticateForWrite(arrivalId)(parse.xml) andThen validateMessageSenderNode.filter andThen validateTransitionState andThen validateOutboundMessage)
      .async {
        implicit request: OutboundMessageRequest[NodeSeq] =>
          val arrival     = request.arrivalRequest.arrival
          val messageType = request.message.messageType.messageType

          arrivalMovementService
            .makeOutboundMessage(arrivalId, arrival.nextMessageCorrelationId, messageType)(request.arrivalRequest.request.body) match {
            case Right(message) =>
              submitMessageService
                .submitMessage(arrivalId, arrival.nextMessageId, message, request.message.nextState, arrival.channel)
                .map {
                  result =>
                    movementSummaryLogger.info(
                      s"Received message ${MessageType.UnloadingRemarks.toString} for this arrival with result $result\n${arrival.summaryInformation
                        .mkString("\n")}")

                    val counter = Monitors.countMessages(messageType, arrival.channel, result)
                    metricsService.inc(counter)

                    result match {
                      case SubmissionFailureInternal => InternalServerError
                      case SubmissionFailureExternal => BadGateway
                      case submissionFailureRejected: SubmissionFailureRejected =>
                        BadRequest(submissionFailureRejected.responseBody)
                      case SubmissionSuccess =>
                        auditService.auditEvent(AuditType.UnloadingRemarksSubmitted, message, arrival.channel)
                        Accepted("Message accepted")
                          .withHeaders("Location" -> routes.MessagesController.getMessage(arrival.arrivalId, arrival.nextMessageId).url)
                    }
                }
            case Left(error) =>
              logger.error(s"Failed to create MovementMessageWithStatus with error: $error")
              Future.successful(BadRequest(s"Failed to create MovementMessageWithStatus with error: $error $messageType"))
          }
      }

  def getMessage(arrivalId: ArrivalId, messageId: MessageId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      val messages = request.arrival.messages.toList

      if (messages.isDefinedAt(messageId.index) && !messages(messageId.index).optStatus.contains(SubmissionFailed))
        Ok(Json.toJsObject(ResponseMovementMessage.build(arrivalId, messageId, messages(messageId.index))))
      else NotFound
  }

  def getMessages(arrivalId: ArrivalId): Action[AnyContent] = authenticateForRead(arrivalId) {
    implicit request =>
      Ok(Json.toJsObject(ResponseArrivalWithMessages.build(request.arrival)))
  }
}
