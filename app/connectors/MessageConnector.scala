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

package connectors

import java.time.OffsetDateTime
import java.util.UUID

import com.google.inject.Inject
import config.AppConfig
import models.request.XMessageType
import play.api.Logger
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import utils.Format

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MessageConnectorImpl @Inject()(config: AppConfig, http: HttpClient) extends MessageConnector {

  def post(xml: String, xMessageType: XMessageType, dateTime: OffsetDateTime)(implicit headerCarrier: HeaderCarrier,
                                                                              ec: ExecutionContext): Future[HttpResponse] = {

    val url = config.eisUrl

    Logger.warn(s"****POST CTC URL $url")

    val newHeaders = headerCarrier.withExtraHeaders(addHeaders(xMessageType, dateTime): _*)

    http.POSTString(url, xml)(rds = HttpReads.readRaw, hc = newHeaders, ec = ec)
  }

  private def addHeaders(xMessageType: XMessageType, dateTime: OffsetDateTime)(implicit headerCarrier: HeaderCarrier): Seq[(String, String)] = {

    val bearerToken   = config.eisBearerToken
    val messageSender = "mdtp-userseori" //TODO: This will be a unique message sender i.e. mdtp-0001-1

    Seq(
      "Content-Type"   -> "application/xml;charset=UTF-8",
      "X-Message-Type" -> xMessageType.code,
      "X-Correlation-ID" -> {
        headerCarrier.sessionId
          .map(_.value)
          .getOrElse(UUID.randomUUID().toString)
      },
      "X-Message-Sender" -> messageSender,
      "X-Forwarded-Host" -> "mdtp",
      "Date"             -> Format.dateFormattedForHeader(dateTime),
      "Accept"           -> "application/xml"
      //"Authorization"    -> s"Bearer $bearerToken"
    )
  }
}

trait MessageConnector {
  def post(xml: String, xMessageType: XMessageType, dateTime: OffsetDateTime)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse]
}
