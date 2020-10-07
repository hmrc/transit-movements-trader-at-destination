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

import audit.AuditService
import audit.AuditType
import cats.data.NonEmptyList
import controllers.actions._
import javax.inject.Inject
import models.MessageStatus.SubmissionSucceeded
import models.ArrivalId
import models.ArrivalStatus
import models.EisSubmissionResult.EisSubmissionRejected
import models.EisSubmissionResult.ErrorInPayload
import models.EisSubmissionResult.VirusFoundOrInvalidToken
import models.MessageType
import models.MovementMessage
import models.ResponseArrivals
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
import services.SubmitMessageService
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
  validateMessageSenderNode: ValidateMessageSenderNodeFilter
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val allMessageUnsent: NonEmptyList[MovementMessage] => Boolean =
    _.map(_.optStatus).forall {
      case Some(messageStatus) if messageStatus != SubmissionSucceeded => true
      case _                                                           => false
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
                  case SubmissionFailureInternal => InternalServerError
                  case SubmissionFailureExternal => BadGateway
                  case SubmissionFailureRejected(submissionResult: EisSubmissionRejected) =>
                    submissionResult match {
                      case ErrorInPayload =>
                        Status(submissionResult.httpStatus)(submissionResult.asString)
                      case VirusFoundOrInvalidToken =>
                        InternalServerError
                    }
                  case SubmissionSuccess =>
                    auditService.auditEvent(AuditType.ArrivalNotificationSubmitted, request.body)
                    Accepted("Message accepted")
                      .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)
                }
            case Left(error) =>
              Logger.error(s"Failed to create ArrivalMovementWithStatus with the following error: $error")
              Future.successful(BadRequest(s"Failed to create ArrivalMovementWithStatus with the following error: $error"))
          }
        case _ =>
          arrivalMovementService
            .makeArrivalMovement(request.eoriNumber, request.body)
            .flatMap {
              case Right(arrival) =>
                submitMessageService
                  .submitArrival(arrival)
                  .map {
                    case SubmissionFailureExternal => BadGateway
                    case SubmissionFailureInternal => InternalServerError
                    case SubmissionFailureRejected(submissionResult: EisSubmissionRejected) =>
                      submissionResult match {
                        case ErrorInPayload =>
                          Status(submissionResult.httpStatus)(submissionResult.asString)
                        case VirusFoundOrInvalidToken =>
                          InternalServerError
                      }
                    case SubmissionSuccess =>
                      auditService.auditEvent(AuditType.ArrivalNotificationSubmitted, request.body)
                      Accepted("Message accepted")
                        .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)
                  }
                  .recover {
                    case _ => {
                      InternalServerError
                    }
                  }
              case Left(error) =>
                Logger.error(s"Failed to create ArrivalMovement with the following error: $error")
                Future.successful(BadRequest(s"Failed to create ArrivalMovement with the following error: $error"))
            }
            .recover {
              case error =>
                Logger.error(s"Failed to create ArrivalMovement with the following error: $error")
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
              case SubmissionFailureInternal => InternalServerError
              case SubmissionFailureExternal => BadGateway
              case SubmissionFailureRejected(submissionResult: EisSubmissionRejected) =>
                submissionResult match {
                  case ErrorInPayload =>
                    Status(submissionResult.httpStatus)(submissionResult.asString)
                  case VirusFoundOrInvalidToken =>
                    InternalServerError
                }
              case SubmissionSuccess =>
                auditService.auditEvent(AuditType.ArrivalNotificationReSubmitted, request.body)
                Accepted("Message accepted")
                  .withHeaders("Location" -> routes.MovementsController.getArrival(request.arrival.arrivalId).url)
            }
        case Left(error) =>
          Logger.error(s"Failed to create message and MovementReferenceNumber with error: $error")
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
        .fetchAllArrivals(request.eoriNumber)
        .map {
          allArrivals =>
            Ok(Json.toJsObject(ResponseArrivals(allArrivals.map {
              arrival =>
                ResponseArrival.build(arrival)
            })))
        }
        .recover {
          case e =>
            InternalServerError(s"Failed with the following error: $e")
        }
  }

}
