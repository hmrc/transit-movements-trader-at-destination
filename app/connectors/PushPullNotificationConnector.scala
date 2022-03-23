/*
 * Copyright 2022 HM Revenue & Customs
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
import models.ArrivalMessageNotification
import models.Box
import models.BoxId
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.UpstreamErrorResponse

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class PushPullNotificationConnector @Inject()(config: AppConfig, http: HttpClient, val metrics: Metrics) extends HasMetrics {

  def getBox(clientId: String)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Either[UpstreamErrorResponse, Box]] =
    withMetricsTimerAsync("get-ppns-box") {
      _ =>
        val url = s"${config.pushPullUrl}/box"
        val queryParams = Seq(
          "boxName"  -> Constants.BoxName,
          "clientId" -> clientId
        )

        http.GET[Either[UpstreamErrorResponse, Box]](url, queryParams)
    }

  def postNotification(boxId: BoxId, notification: ArrivalMessageNotification)(implicit ec: ExecutionContext,
                                                                               hc: HeaderCarrier): Future[Either[UpstreamErrorResponse, Unit]] =
    withMetricsTimerAsync("post-ppns-notification") {
      _ =>
        val url = s"${config.pushPullUrl}/box/${boxId.value}/notifications"
        val headers = Seq(
          HeaderNames.CONTENT_TYPE -> ContentTypes.JSON
        )

        http.POST[ArrivalMessageNotification, Either[UpstreamErrorResponse, Unit]](url, notification, headers)
    }
}
