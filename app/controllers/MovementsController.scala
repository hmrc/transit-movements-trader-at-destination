/*
 * Copyright 2023 HM Revenue & Customs
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
import config.Constants
import controllers.actions._
import logging.Logging
import metrics.HasActionMetrics
import metrics.Monitors
import metrics.WeekLongCounter
import models.ArrivalId
import models.Box
import models.ChannelType
import models.EnrolmentId
import models.MessageType
import models.MovementMessage
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult._
import models.request.ArrivalRequest
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

import java.time.Clock
import java.time.OffsetDateTime
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementsController @Inject() (
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementMessageService,
  submitMessageService: SubmitMessageService,
  authenticate: AuthenticateActionProvider,
  authenticatedArrivalForReadWithoutMessages: AuthenticatedGetArrivalWithoutMessagesForReadActionProvider,
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

  lazy val countArrivals               = histo("get-all-arrivals-count")
  lazy val messagesCount               = histo("get-arrival-by-id-messages-count")
  lazy val acceptedArrivalsWithPPNSBox = new WeekLongCounter(Clock.systemDefaultZone(), counter("accepted-arrivals-with-ppns-box"))
  lazy val acceptedArrivals            = new WeekLongCounter(Clock.systemDefaultZone(), counter("accepted-arrivals"))

  private def handleSubmissionResult(
    result: SubmissionProcessingResult,
    arrivalNotificationType: String,
    message: MovementMessage,
    requestChannel: ChannelType,
    arrivalId: ArrivalId,
    enrolmentId: EnrolmentId,
    boxOpt: Option[Box],
    requestLength: Int
  )(implicit hc: HeaderCarrier) =
    result match {
      case SubmissionFailureInternal => InternalServerError
      case SubmissionFailureExternal => BadGateway
      case submissionFailureRejected: SubmissionFailureRejected =>
        BadRequest(submissionFailureRejected.responseBody)
      case SubmissionSuccess =>
        auditService.auditArrivalNotificationWithStatistics(
          arrivalNotificationType,
          enrolmentId.customerId,
          enrolmentId.enrolmentType,
          message,
          requestChannel,
          requestLength,
          boxOpt.map(_.boxId)
        )
        auditService.auditArrivalNotificationWithStatistics(
          AuditType.MesSenMES3Added,
          enrolmentId.customerId,
          enrolmentId.enrolmentType,
          message,
          requestChannel,
          requestLength,
          None
        )
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
      (authenticate() andThen validateMessageSenderNode.filter).async(parse.xml) {
        implicit request =>
          getBox(request.headers.get(Constants.XClientIdHeader)).flatMap {
            boxOpt =>
              arrivalMovementService
                .makeArrivalMovement(request.enrolmentId, request.body, request.channel, boxOpt)
                .flatMap {
                  case Right(arrival) =>
                    submitMessageService
                      .submitArrival(arrival)
                      .map {
                        result =>
                          Monitors.messageReceived(registry, MessageType.ArrivalNotification, request.channel, result)
                          movementSummaryLogger.info(s"Submitted an arrival with result ${result.toString}\n${arrival.summaryInformation.mkString("\n")}")
                          if (result == SubmissionSuccess) {
                            if (boxOpt.isDefined) acceptedArrivalsWithPPNSBox.inc()
                            acceptedArrivals.inc()
                          }
                          handleSubmissionResult(
                            result,
                            AuditType.ArrivalNotificationSubmitted,
                            arrival.messages.head,
                            request.channel,
                            arrival.arrivalId,
                            request.enrolmentId,
                            boxOpt,
                            request.length
                          )
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

  def putArrival(arrivalId: ArrivalId): Action[NodeSeq] =
    withMetricsTimerAction("put-arrival") {
      authenticateForWrite(arrivalId).async(parse.xml) {
        implicit request: ArrivalRequest[NodeSeq] =>
          arrivalMovementService
            .messageAndMrn(arrivalId, request.arrival.nextMessageId, request.arrival.nextMessageCorrelationId)(request.body) match {
            case Right((message, mrn)) =>
              submitMessageService
                .submitIe007Message(arrivalId, message, mrn, request.channel)
                .map {
                  result =>
                    movementSummaryLogger.info(s"Submitted an arrival with result ${result.toString}\n${request.arrival.summaryInformation.mkString("\n")}")
                    handleSubmissionResult(
                      result,
                      AuditType.ArrivalNotificationReSubmitted,
                      message,
                      request.channel,
                      request.arrival.arrivalId,
                      request.request.enrolmentId,
                      request.arrival.notificationBox,
                      request.request.length
                    )
                }
            case Left(error) =>
              logger.error(s"Failed to create message and MovementReferenceNumber with error: $error")
              Future.successful(BadRequest(s"Failed to create message and MovementReferenceNumber with error: $error"))
          }
      }
    }

  def getArrival(arrivalId: ArrivalId): Action[AnyContent] =
    withMetricsTimerAction("get-arrival-by-id") {
      authenticatedArrivalForReadWithoutMessages(arrivalId) {
        implicit request =>
          Ok(Json.toJsObject(ResponseArrival.build(request.arrivalWithoutMessages)))
      }
    }

  def getArrivals(updatedSince: Option[OffsetDateTime], mrn: Option[String], pageSize: Option[Int] = None, page: Option[Int] = None): Action[AnyContent] =
    withMetricsTimerAction("get-all-arrivals") {
      authenticate().async {
        implicit request =>
          arrivalMovementRepository
            .fetchAllArrivals(request.enrolmentId.value, request.channel, updatedSince, mrn, pageSize, page)
            .map {
              responseArrivals =>
                countArrivals.update(responseArrivals.retrievedArrivals)
                Ok(Json.toJsObject(responseArrivals))
            }
            .recover {
              case e =>
                InternalServerError(s"Failed with the following error: $e")
            }
      }
    }

}
