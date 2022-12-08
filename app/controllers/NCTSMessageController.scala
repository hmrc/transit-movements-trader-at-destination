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

import com.kenshoo.play.metrics.Metrics
import logging.Logging
import metrics.HasActionMetrics
import models.ArrivalNotFoundError
import models.DocumentExistsError
import models.InboundMessageRequest
import models.InternalError
import models.MessageSender
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services._
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class NCTSMessageController @Inject()(
  cc: ControllerComponents,
  inboundRequestSubmissionService: MovementMessageOrchestratorService,
  lockService: LockService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  private val movementSummaryLogger: Logger = Logger(s"application.${this.getClass.getCanonicalName}.movementSummary")

  def post(messageSender: MessageSender): Action[NodeSeq] =
    Action(parse.xml).async {
      implicit request =>
        withMetricsTimerResult("post-receive-ncts-message") {
          inboundRequestSubmissionService.saveNCTSMessage(messageSender, request.body, request.headers).flatMap {
            case Left(submissionState) =>
              lockService.unlock(messageSender.arrivalId).map {
                _ =>
                  submissionState match {
                    case _: ArrivalNotFoundError => NotFound
                    case _: DocumentExistsError  => Locked
                    case state: InternalError =>
                      logger.error(state.message)
                      InternalServerError
                    case state =>
                      logger.warn(state.message)
                      BadRequest
                  }
              }
            case Right(InboundMessageRequest(arrival, inboundMessageResponse, _)) =>
              val summaryInfo: Map[String, String] = Map(
                "X-Correlation-Id" -> request.headers.get("X-Correlation-ID").getOrElse("undefined"),
                "X-Request-Id"     -> request.headers.get("X-Request-ID").getOrElse("undefined")
              ) ++ arrival.summaryInformation

              movementSummaryLogger.info(s"Received message ${inboundMessageResponse.messageType.toString} for this arrival\n${summaryInfo.mkString("\n")}")

              Future.successful(Ok.withHeaders(LOCATION -> routes.MessagesController.getMessage(arrival.arrivalId, arrival.nextMessageId).url))
          }
        }
    }
}
