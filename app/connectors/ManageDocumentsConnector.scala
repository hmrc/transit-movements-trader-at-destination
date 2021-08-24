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
import com.kenshoo.play.metrics.Metrics
import config.AppConfig
import config.Constants
import metrics.HasMetrics
import models.response.ResponseMovementMessage
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ManageDocumentsConnector @Inject() (config: AppConfig, ws: WSClient, val metrics: Metrics)(implicit ec: ExecutionContext) extends HasMetrics {

  def getUnloadingPermissionPdf(movementMessage: ResponseMovementMessage)(implicit hc: HeaderCarrier): Future[WSResponse] = {
    val serviceUrl = s"${config.manageDocumentsUrl}/unloading-permission"

    val headers = hc.headers(Seq(Constants.XClientIdHeader, Constants.XRequestIdHeader)) ++ Seq(
      HeaderNames.CONTENT_TYPE -> ContentTypes.XML,
      HeaderNames.USER_AGENT   -> config.appName
    )

    withMetricsTimerResponse("manage-documents-get-unloading-permission-pdf") {
      ws.url(serviceUrl)
        .withHttpHeaders(headers: _*)
        .post(movementMessage.message.toString)
    }
  }
}
