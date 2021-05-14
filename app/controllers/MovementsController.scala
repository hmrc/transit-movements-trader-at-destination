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

import javax.inject.Inject
import java.time.OffsetDateTime

import audit.AuditService
import audit.AuditType
import cats.data.NonEmptyList
import com.kenshoo.play.metrics.Metrics
import controllers.actions._
import logging.Logging
import metrics.Monitors
import metrics.HasActionMetrics
import models.MessageStatus.SubmissionSucceeded
import models.ArrivalId
import models.ArrivalStatus
import models.Box
import models.ChannelType
import models.MessageType
import models.MovementMessage
import models.ResponseArrivals
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult._
import models.request.ArrivalRequest
import models.request.AuthenticatedClientRequest
import models.request.AuthenticatedOptionalArrivalRequest
import models.response.ResponseArrival
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementMessageService
import services.PushPullNotificationService
import services.SubmitMessageService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementsController @Inject()(
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementMessageService,
  submitMessageService: SubmitMessageService,
  authenticate: AuthenticateActionProvider,
  authenticateClientId: AuthenticatedClientIdActionProvider,
  authenticatedArrivalForRead: AuthenticatedGetArrivalForReadActionProvider,
  authenticatedOptionalArrival: AuthenticatedGetOptionalArrivalForWriteActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  auditService: AuditService,
  validateMessageSenderNode: ValidateMessageSenderNodeFilter,
  pushPullNotificationService: PushPullNotificationService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  private val movementSummaryLogger: Logger =
    Logger(s"application.${this.getClass.getCanonicalName}.movementSummary")

  private val allMessageUnsent: NonEmptyList[MovementMessage] => Boolean =
    _.map(_.optStatus).forall {
      case Some(messageStatus) if messageStatus != SubmissionSucceeded => true
      case _                                                           => false
    }

  lazy val countArrivals = histo("get-all-arrivals-count")
  lazy val messagesCount = histo("get-arrival-by-id-messages-count")

  private def handleSubmissionResult(
    result: SubmissionProcessingResult,
    arrivalNotificationType: String,
    message: MovementMessage,
    requestChannel: ChannelType,
    arrivalId: ArrivalId
  )(implicit hc: HeaderCarrier) =
    result match {
      case SubmissionFailureInternal => InternalServerError
      case SubmissionFailureExternal => BadGateway
      case submissionFailureRejected: SubmissionFailureRejected =>
        BadRequest(submissionFailureRejected.responseBody)
      case SubmissionSuccess =>
        auditService.auditEvent(arrivalNotificationType, message, requestChannel)
        auditService.auditEvent(AuditType.MesSenMES3Added, message, requestChannel)
        Accepted("Message Accepted").withHeaders("Location" -> routes.MovementsController.getArrival(arrivalId).url)
    }

  private def handleSubmissionResult(
    result: SubmissionProcessingResult,
    arrivalNotificationType: String,
    message: MovementMessage,
    requestChannel: ChannelType,
    arrivalId: ArrivalId,
    boxOpt: Option[Box]
  )(implicit hc: HeaderCarrier) =
    result match {
      case SubmissionFailureInternal => InternalServerError
      case SubmissionFailureExternal => BadGateway
      case submissionFailureRejected: SubmissionFailureRejected =>
        BadRequest(submissionFailureRejected.responseBody)
      case SubmissionSuccess =>
        auditService.auditEvent(arrivalNotificationType, message, requestChannel)
        auditService.auditEvent(AuditType.MesSenMES3Added, message, requestChannel)
        Accepted(Json.toJson(boxOpt)).withHeaders("Location" -> routes.MovementsController.getArrival(arrivalId).url)
    }

  private def getBox(clientIdOpt: Option[String])(implicit hc: HeaderCarrier): Future[Option[Box]] =
    clientIdOpt
      .map {
        clientId =>
          pushPullNotificationService.getBox(clientId)
      }
      .getOrElse(Future.successful(None))

  def post: Action[NodeSeq] =
    withMetricsTimerAction("post-create-arrival") {
      (authenticateClientId() andThen authenticatedOptionalArrival() andThen validateMessageSenderNode.filter).async(parse.xml) {
        implicit request =>
          getBox(request.clientIdOpt).flatMap {
            boxOpt =>
              {
                (boxOpt, request.arrival) match {
                  case (_, Some(arrival)) if allMessageUnsent(arrival.messages) =>
                    arrivalMovementService
                      .makeOutboundMessage(arrival.arrivalId, arrival.nextMessageCorrelationId, MessageType.ArrivalNotification)(request.body) match {
                      case Right(message) =>
                        submitMessageService
                          .submitMessage(arrival.arrivalId, arrival.nextMessageId, message, ArrivalStatus.ArrivalSubmitted, request.channel)
                          .map {
                            result =>
                              Monitors.messageReceived(registry, MessageType.ArrivalNotification, request.channel, result)
                              movementSummaryLogger.info(s"Submitted an arrival with result ${result.toString}\n${arrival.summaryInformation.mkString("\n")}")
                              handleSubmissionResult(result, AuditType.ArrivalNotificationSubmitted, message, request.channel, arrival.arrivalId, boxOpt)
                          }
                      case Left(error) =>
                        logger.error(s"Failed to create ArrivalMovementWithStatus with the following error: $error")
                        Future.successful(BadRequest(s"Failed to create ArrivalMovementWithStatus with the following error: $error"))
                    }
                  case _ =>
                    arrivalMovementService
                      .makeArrivalMovement(request.eoriNumber, request.body, request.channel, boxOpt)
                      .flatMap {
                        case Right(arrival) =>
                          submitMessageService
                            .submitArrival(arrival)
                            .map {
                              result =>
                                Monitors.messageReceived(registry, MessageType.ArrivalNotification, request.channel, result)
                                movementSummaryLogger.info(s"Submitted an arrival with result ${result.toString}\n${arrival.summaryInformation.mkString("\n")}")
                                handleSubmissionResult(result,
                                                       AuditType.ArrivalNotificationSubmitted,
                                                       arrival.messages.head,
                                                       request.channel,
                                                       arrival.arrivalId)
                            }
                            .recover {
                              case _ =>
                                InternalServerError
                            }
                        case Left(error) =>
                          logger.error(s"Failed to create ArrivalMovement with the following error: $error")
                          Future.successful(BadRequest(s"Failed to create ArrivalMovement with the following error: $error"))
                      }
                      .recover {
                        case error =>
                          logger.error(s"Failed to create ArrivalMovement with the following error: $error")
                          InternalServerError
                      }
                }
              }
          }
      }

    }

  def putArrival(arrivalId: ArrivalId): Action[NodeSeq] =
    withMetricsTimerAction("put-arrival") {
      authenticateForWrite(arrivalId).async(parse.xml) {
        implicit request: ArrivalRequest[NodeSeq] =>
          arrivalMovementService
            .messageAndMrn(arrivalId, request.arrival.nextMessageCorrelationId)(request.body) match {
            case Right((message, mrn)) =>
              submitMessageService
                .submitIe007Message(arrivalId, request.arrival.nextMessageId, message, mrn, request.channel)
                .map {
                  result =>
                    movementSummaryLogger.info(s"Submitted an arrival with result ${result.toString}\n${request.arrival.summaryInformation.mkString("\n")}")
                    handleSubmissionResult(result,
                                           AuditType.ArrivalNotificationReSubmitted,
                                           message,
                                           request.channel,
                                           request.arrival.arrivalId,
                                           request.arrival.notificationBox)
                }
            case Left(error) =>
              logger.error(s"Failed to create message and MovementReferenceNumber with error: $error")
              Future.successful(BadRequest(s"Failed to create message and MovementReferenceNumber with error: $error"))
          }
      }
    }

  def getArrival(arrivalId: ArrivalId): Action[AnyContent] =
    withMetricsTimerAction("get-arrival-by-id") {
      authenticatedArrivalForRead(arrivalId) {
        implicit request =>
          messagesCount.update(request.arrival.messages.length)
          Ok(Json.toJsObject(ResponseArrival.build(request.arrival)))
      }
    }

  def getArrivals(updatedSince: Option[OffsetDateTime]): Action[AnyContent] =
    withMetricsTimerAction("get-all-arrivals") {
      authenticate().async {
        implicit request =>
          arrivalMovementRepository
            .fetchAllArrivals(request.eoriNumber, request.channel, updatedSince)
            .map {
              allArrivals =>
                countArrivals.update(allArrivals.length)
                Ok(Json.toJsObject(ResponseArrivals(allArrivals)))
            }
            .recover {
              case e =>
                InternalServerError(s"Failed with the following error: $e")
            }
      }
    }

}
