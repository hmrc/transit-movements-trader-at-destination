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

import com.kenshoo.play.metrics.Metrics
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import logging.Logging
import metrics.HasActionMetrics
import models.ArrivalId
import models.WSError._
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.UnloadingPermissionPDFService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext

class PDFGenerationController @Inject()(
  cc: ControllerComponents,
  authenticateForRead: AuthenticatedGetArrivalForReadActionProvider,
  unloadingPermissionPDFService: UnloadingPermissionPDFService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  @nowarn("msg=match may not be exhaustive.")
  def getPDF(arrivalId: ArrivalId): Action[AnyContent] =
    withMetricsTimerAction("get-unloading-permission-pdf") {
      authenticateForRead(arrivalId).async {
        implicit request =>
          unloadingPermissionPDFService.getPDF(request.arrival).map {
            case Right((pdf, headers)) =>
              Ok(pdf).withHeaders(headers: _*)
            case Left(NotFoundError) =>
              logger.error(s"Failed to find UnloadingPermission of index: ${request.arrival.arrivalId} ")
              NotFound
            case Left(otherError: OtherError) =>
              logger.error(s"Failed create PDF for the following reason: ${otherError.code} - ${otherError.reason}")
              BadGateway
          }
      }
    }
}
