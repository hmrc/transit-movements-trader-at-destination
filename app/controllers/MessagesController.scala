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
import com.kenshoo.play.metrics.Metrics
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import controllers.actions.MessageTransformerInterface
import controllers.actions._
import logging.Logging
import metrics.HasActionMetrics
import metrics.Monitors
import models.ArrivalId
import models.MessageId
import models.MessageStatus.SubmissionFailed
import models.MessageType
import models.SubmissionProcessingResult._
import models.response.ResponseArrivalWithMessages
import models.response.ResponseMovementMessage
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementMessageService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import java.time.OffsetDateTime

import cats.data.OptionT
import javax.inject.Inject
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessagesController @Inject()(
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementMessageService,
  auditService: AuditService,
  authenticate: AuthenticateActionProvider,
  authenticateForReadWithMessages: AuthenticatedGetArrivalForReadActionProvider,
  authenticateForWriteWithMessages: AuthenticatedGetArrivalForWriteActionProvider,
  authenticateForWriteWithoutMessages: AuthenticatedGetArrivalWithoutMessagesForWriteActionProvider,
  cc: ControllerComponents,
  submitMessageService: SubmitMessageService,
  validateMessageSenderNode: ValidateMessageSenderNodeFilter,
  validateOutboundMessage: ValidateOutboundMessageAction,
  validateTransitionState: MessageTransformerInterface,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  private val movementSummaryLogger: Logger =
    Logger(s"application.${this.getClass.getCanonicalName}.movementSummary")

  lazy val countMessages = histo("get-all-arrival-messages-count")

  def post(arrivalId: ArrivalId): Action[NodeSeq] =
    withMetricsTimerAction("post-submit-message") {
      (authenticateForWriteWithoutMessages(arrivalId)(parse.xml) andThen validateMessageSenderNode.filter andThen validateTransitionState andThen validateOutboundMessage)
        .async {
          implicit request: OutboundMessageRequest[NodeSeq] =>
            val arrival     = request.arrivalWithoutMessageRequest.arrivalWithoutMessages
            val messageType = request.message.messageType.messageType
            arrivalMovementService
              .makeOutboundMessage(arrivalId, arrival.nextMessageId, arrival.nextMessageCorrelationId, messageType)(
                request.arrivalWithoutMessageRequest.request.body) match {
              case Right(message) =>
                submitMessageService
                  .submitMessage(arrivalId, arrival.nextMessageId, message, request.message.nextState, arrival.channel)
                  .map {
                    result =>
                      movementSummaryLogger.info(
                        s"Received message ${MessageType.UnloadingRemarks.toString} for this arrival with result $result\n${arrival.summaryInformation
                          .mkString("\n")}"
                      )

                      Monitors.messageReceived(registry, messageType, arrival.channel, result)

                      result match {
                        case SubmissionFailureInternal => InternalServerError
                        case SubmissionFailureExternal => BadGateway
                        case submissionFailureRejected: SubmissionFailureRejected =>
                          BadRequest(submissionFailureRejected.responseBody)
                        case SubmissionSuccess =>
                          auditService.auditEvent(AuditType.UnloadingRemarksSubmitted, arrival.eoriNumber, message, arrival.channel)
                          Accepted("Message accepted")
                            .withHeaders("Location" -> routes.MessagesController.getMessage(arrival.arrivalId, arrival.nextMessageId).url)
                      }
                  }
              case Left(error) =>
                logger.error(s"Failed to create MovementMessageWithStatus with error: $error")
                Future.successful(BadRequest(s"Failed to create MovementMessageWithStatus with error: $error $messageType"))
            }
        }
    }

  def getMessage(arrivalId: ArrivalId, messageId: MessageId): Action[AnyContent] =
    withMetricsTimerAction("get-arrival-message") {
      authenticate().async {
        implicit request =>
          val result = for {
            arrival <- OptionT(arrivalMovementRepository.getWithoutMessages(arrivalId, request.channel))
            if arrival.eoriNumber == request.eoriNumber
            message <- OptionT(arrivalMovementRepository.getMessage(arrivalId, request.channel, messageId))
            if message.optStatus != Some(SubmissionFailed)
          } yield Ok(Json.toJsObject(ResponseMovementMessage.build(arrivalId, messageId, message)))

          result.getOrElse(NotFound)
      }

    }

  def getMessages(arrivalId: ArrivalId, receivedSince: Option[OffsetDateTime]): Action[AnyContent] =
    withMetricsTimerAction("get-all-arrival-messages") {
      authenticateForReadWithMessages(arrivalId) {
        implicit request =>
          val response = ResponseArrivalWithMessages.build(request.arrival, receivedSince)
          countMessages.update(response.messages.length)
          Ok(Json.toJsObject(response))
      }
    }
}
