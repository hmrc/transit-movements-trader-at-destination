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

package connectors

import com.google.inject.Inject
import config.AppConfig
import metrics.MetricsService
import metrics.Monitors
import models.response.ResponseMovementMessage
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ManageDocumentsConnector @Inject()(config: AppConfig, ws: WSClient, metricsService: MetricsService)(implicit ec: ExecutionContext) {

  def getUnloadingPermissionPdf(movementMessage: ResponseMovementMessage)(implicit hc: HeaderCarrier): Future[WSResponse] = {

    val serviceUrl = s"${config.manageDocumentsUrl}/unloading-permission"
    val headers    = ("Content-Type", "application/xml")

    metricsService.timeAsyncCall(Monitors.UnloadingPermissionMonitor) {
      ws.url(serviceUrl)
        .withHttpHeaders(headers)
        .post(movementMessage.message.toString)
    }
  }
}
