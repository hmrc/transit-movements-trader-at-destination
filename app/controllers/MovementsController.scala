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

import cats.data.NonEmptyList
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalForWriteActionProvider
import controllers.actions.AuthenticatedGetOptionalArrivalForWriteActionProvider
import javax.inject.Inject
import models.MessageStatus.SubmissionSucceeded
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.MessageSender
import models.MessageType
import models.MovementMessage
import models.ResponseArrivals
import models.SubmissionProcessingResult
import models.request.ArrivalRequest
import models.response.ResponseArrival
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.DefaultActionBuilder
import repositories.ArrivalMovementRepository
import services.ArrivalMovementMessageService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController
import utils.XMLTransformer

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
  defaultActionBuilder: DefaultActionBuilder
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  private val allMessageUnsent: NonEmptyList[MovementMessage] => Boolean =
    _.map(_.optStatus).forall {
      case Some(messageStatus) if messageStatus != SubmissionSucceeded => true
      case _                                                           => false
    }

  def post: Action[NodeSeq] = authenticatedOptionalArrival().async(parse.xml) {
    implicit request =>
      request.arrival match {
        case Some(arrival) if allMessageUnsent(arrival.messages) =>
          arrivalMovementService
            .makeMovementMessageWithStatus(arrival.nextMessageCorrelationId, MessageType.ArrivalNotification)(getUpdatedRequestBody(arrival, request.body))
            .map {
              message =>
                submitMessageService
                  .submitMessage(arrival.arrivalId, arrival.nextMessageId, message, ArrivalStatus.ArrivalSubmitted)
                  .map {
                    case SubmissionProcessingResult.SubmissionSuccess =>
                      Accepted("Message accepted")
                        .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)

                    case SubmissionProcessingResult.SubmissionFailureInternal =>
                      InternalServerError

                    case SubmissionProcessingResult.SubmissionFailureExternal =>
                      BadGateway
                  }
            }
            .getOrElse {
              Logger.warn("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5")
              Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
            }

        case _ =>
          arrivalMovementService.makeArrivalMovement(request.eoriNumber)(request.body) match {
            case None =>
              Logger.warn("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5")
              Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
            case Some(arrivalFuture) =>
              arrivalFuture
                .flatMap {
                  arrival =>
                    submitMessageService.submitArrival(arrival).map {
                      case SubmissionProcessingResult.SubmissionSuccess =>
                        Accepted("Message accepted")
                          .withHeaders("Location" -> routes.MovementsController.getArrival(arrival.arrivalId).url)
                      case SubmissionProcessingResult.SubmissionFailureExternal =>
                        BadGateway
                      case SubmissionProcessingResult.SubmissionFailureInternal =>
                        InternalServerError
                    }
                }
                .recover {
                  case _ => {
                    InternalServerError
                  }
                }
          }
      }
  }

  def putArrival(arrivalId: ArrivalId): Action[NodeSeq] = authenticateForWrite(arrivalId).async(parse.xml) {
    implicit request: ArrivalRequest[NodeSeq] =>
      arrivalMovementService
        .messageAndMrn(request.arrival.nextMessageCorrelationId)(getUpdatedRequestBody(request.arrival, request.body))
        .map {
          case (message, mrn) =>
            submitMessageService
              .submitIe007Message(arrivalId, request.arrival.nextMessageId, message, mrn)
              .map {
                case SubmissionProcessingResult.SubmissionSuccess =>
                  Accepted("Message accepted")
                    .withHeaders("Location" -> routes.MovementsController.getArrival(request.arrival.arrivalId).url)

                case SubmissionProcessingResult.SubmissionFailureInternal =>
                  InternalServerError

                case SubmissionProcessingResult.SubmissionFailureExternal =>
                  BadGateway
              }
        }
        .getOrElse {
          Logger.warn("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5")
          Future.successful(BadRequest("Invalid data: missing either DatOfPreMES9, TimOfPreMES10 or DocNumHEA5"))
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
  private def getUpdatedRequestBody(arrival: Arrival, body: NodeSeq): NodeSeq = {
    // println(s"---------------${request.body}")
    val messageSender: MessageSender = MessageSender(arrival.arrivalId, arrival.nextMessageCorrelationId)
    val foo                          = XMLTransformer.addXmlNode("SynVerNumMES2", "MesSenMES3", messageSender.toString, body)
    println(s"*****$foo")
    foo
  }

}
