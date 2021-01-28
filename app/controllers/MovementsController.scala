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
import cats.data.NonEmptyList
import controllers.actions._

import javax.inject.Inject
import logging.Logging
import metrics.MetricsService
import metrics.Monitors
import models.MessageStatus.SubmissionSucceeded
import models.ArrivalId
import models.ArrivalStatus
import models.ChannelType
import models.MessageType
import models.MovementMessage
import models.ResponseArrivals
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult._
import models.request.ArrivalRequest
import models.response.ResponseArrival
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.ArrivalMovementRepository
import services.ArrivalMovementMessageService
import services.SubmitMessageService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementsController @Inject()(
  cc: ControllerComponents,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalMovementService: ArrivalMovementMessageService,
  submitMessageService: SubmitMessageService,
  authenticate: AuthenticateActionProvider,
  authenticatedArrivalForRead: AuthenticatedGetArrivalForReadActionProvider,
  authenticatedOptionalArrival: AuthenticatedGetOptionalArrivalForWriteActionProvider,
  authenticateForWrite: AuthenticatedGetArrivalForWriteActionProvider,
  auditService: AuditService,
  validateMessageSenderNode: ValidateMessageSenderNodeFilter,
  metricsService: MetricsService
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  private val allMessageUnsent: NonEmptyList[MovementMessage] => Boolean =
    _.map(_.optStatus).forall {
      case Some(messageStatus) if messageStatus != SubmissionSucceeded => true
      case _                                                           => false
    }

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
        Accepted("Message accepted")
          .withHeaders("Location" -> routes.MovementsController.getArrival(arrivalId).url)
    }

  def post: Action[NodeSeq] = (authenticatedOptionalArrival()(parse.xml) andThen validateMessageSenderNode.filter).async {
    implicit request =>
      request.arrival match {
        case Some(arrival) if allMessageUnsent(arrival.messages) =>
          arrivalMovementService
            .makeOutboundMessage(arrival.arrivalId, arrival.nextMessageCorrelationId, MessageType.ArrivalNotification)(request.body) match {
            case Right(message) =>
              submitMessageService
                .submitMessage(arrival.arrivalId, arrival.nextMessageId, message, ArrivalStatus.ArrivalSubmitted)
                .map {
                  result =>
                    val counter = Monitors.countMessages(MessageType.ArrivalNotification, request.channel, result)
                    metricsService.inc(counter)

                    handleSubmissionResult(result, AuditType.ArrivalNotificationSubmitted, message, request.channel, arrival.arrivalId)

                }
            case Left(error) =>
              logger.error(s"Failed to create ArrivalMovementWithStatus with the following error: $error")
              Future.successful(BadRequest(s"Failed to create ArrivalMovementWithStatus with the following error: $error"))
          }
        case _ =>
          arrivalMovementService
            .makeArrivalMovement(request.eoriNumber, request.body, request.channel)
            .flatMap {
              case Right(arrival) =>
                submitMessageService
                  .submitArrival(arrival)
                  .map {
                    result =>
                      handleSubmissionResult(result, AuditType.ArrivalNotificationSubmitted, arrival.messages.head, request.channel, arrival.arrivalId)
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

  def putArrival(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      arrivalMovementService
        .messageAndMrn(arrivalId, request.arrival.nextMessageCorrelationId)(request.body) match {
        case Right((message, mrn)) =>
          submitMessageService
            .submitIe007Message(arrivalId, request.arrival.nextMessageId, message, mrn)
            .map {
              result =>
                handleSubmissionResult(result, AuditType.ArrivalNotificationReSubmitted, message, request.channel, request.arrival.arrivalId)
            }
        case Left(error) =>
          logger.error(s"Failed to create message and MovementReferenceNumber with error: $error")
          Future.successful(BadRequest(s"Failed to create message and MovementReferenceNumber with error: $error"))
      }
  }

  def getArrival(arrivalId: ArrivalId): Action[AnyContent] = authenticatedArrivalForRead(arrivalId) {
    implicit request =>
      Ok(Json.toJsObject(ResponseArrival.build(request.arrival)))
  }

  def getArrivals(): Action[AnyContent] = authenticate().async {
    implicit request =>
      arrivalMovementRepository
        .fetchAllArrivals(request.eoriNumber, request.channel)
        .map {
          allArrivals =>
            Ok(Json.toJsObject(ResponseArrivals(allArrivals)))
        }
        .recover {
          case e =>
            InternalServerError(s"Failed with the following error: $e")
        }
  }

}
