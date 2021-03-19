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

import controllers.actions.GetArrivalForWriteActionProvider
import javax.inject.Inject
import logging.Logging
import metrics.MetricsService
import metrics.Monitors
import models.Message
import models.MessageSender
import models.SubmissionProcessingResult._
import models.request.actions.MessageTransformerInterface
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
                                      inboundMessage: MessageTransformerInterface,
                                      saveMessageService: SaveMessageService,
                                      metricsService: MetricsService)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  private val movementSummaryLogger: Logger =
    Logger(s"application.${this.getClass.getCanonicalName}.movementSummary")

  def post(messageSender: MessageSender): Action[NodeSeq] = (getArrival(messageSender.arrivalId)(parse.xml) andThen inboundMessage).async {
    implicit request =>
      val messageInbound: Message = request.message

      val xml: NodeSeq = request.arrivalRequest.request.body

      val processingResult =
        saveMessageService.validateXmlAndSaveMessage(xml, messageSender, messageInbound.messageType, messageInbound.nextState, request.arrivalRequest.channel)

      processingResult map {
        result =>
          val summaryInfo: Map[String, String] = Map(
            "X-Correlation-Id" -> request.headers.get("X-Correlation-ID").getOrElse("undefined"),
            "X-Request-Id"     -> request.headers.get("X-Request-ID").getOrElse("undefined")
          ) ++ request.arrivalRequest.arrival.summaryInformation

          movementSummaryLogger.info(s"Received message ${messageInbound.messageType.messageType.toString} for this arrival\n${summaryInfo.mkString("\n")}")

          val counter = Monitors.countMessages(messageInbound.messageType.messageType, request.arrivalRequest.channel, result)
          metricsService.inc(counter)

          result match {
            case SubmissionSuccess =>
              Ok.withHeaders(
                LOCATION -> routes.MessagesController.getMessage(request.arrivalRequest.arrival.arrivalId, request.arrivalRequest.arrival.nextMessageId).url)
            case SubmissionFailureInternal => internalServerError("Internal Submission Failure " + processingResult)
            case SubmissionFailureExternal => badRequestError("External Submission Failure " + processingResult)
          }
      }
  }

  private def internalServerError(message: String): Result = {
    logger.error(message)
    InternalServerError(message)
  }

  private def badRequestError(message: String): Result = {
    logger.warn(message)
    BadRequest(message)
  }

}
