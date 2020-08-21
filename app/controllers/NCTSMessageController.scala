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
import controllers.actions.GetArrivalForWriteActionProvider
import javax.inject.Inject
import models.MessageInbound
import models.MessageSender
import models.SubmissionProcessingResult._
import models.request.actions.InboundMessageTransformerInterface
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import play.api.mvc.Result
import services.SaveMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq

class NCTSMessageController @Inject()(cc: ControllerComponents,
                                      getArrival: GetArrivalForWriteActionProvider,
                                      inboundMessage: InboundMessageTransformerInterface,
                                      auditService: AuditService,
                                      saveMessageService: SaveMessageService)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post(messageSender: MessageSender): Action[NodeSeq] = (getArrival(messageSender.arrivalId)(parse.xml) andThen inboundMessage).async {
    implicit request =>
      val messageInbound: MessageInbound = request.inboundMessage
      val xml: NodeSeq                   = request.request.request.body
      val processingResult               = saveMessageService.validateXmlAndSaveMessage(xml, messageSender, messageInbound.messageType, messageInbound.nextState)

      processingResult map {
        case SubmissionSuccess =>
          auditService.auditNCTSMessages(messageInbound.messageType, xml)
          Ok
        case SubmissionFailureInternal => internalServerError("Internal Submission Failure " + processingResult)
        case SubmissionFailureExternal => badRequestError("External Submission Failure " + processingResult)
      }

  }

  //TODO: Should we log and return all 400/500s from a single place?
  private def internalServerError(message: String): Result = {
    Logger.error(message)
    InternalServerError(message)
  }

  private def badRequestError(message: String): Result = {
    Logger.warn(message)
    BadRequest(message)
  }

}
