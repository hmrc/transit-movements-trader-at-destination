/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import com.google.inject.Inject
import config.AppConfig
import models.messages.request.XMessageType
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MessageConnectorImpl @Inject()(config: AppConfig, http: HttpClient) extends MessageConnector {

  def post(xml: String, xMessageType: XMessageType, dateTime: OffsetDateTime)(implicit headerCarrier: HeaderCarrier): Future[HttpResponse] = {
    
    val url                              = config.eisUrl
    val messageSender                    = "mdtp-userseori"
    val dateFormatter: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME
    val dateTimeFormatted: String        = dateTime.format(dateFormatter)

    val customHeaders: Seq[(String, String)] = Seq(
      "Content-Type"   -> "application/xml;charset=UTF-8",
      "X-Message-Type" -> xMessageType.code,
      "X-Correlation-ID" -> {
        headerCarrier.sessionId
          .map(_.value)
          .getOrElse(UUID.randomUUID().toString)
      },
      "X-Message-Sender" -> messageSender,
      "X-Forwarded-Host" -> "mdtp",
      "Date"             -> dateTimeFormatted,
      "Accept"           -> "application/xml",
      "Authorisation"    -> config.bearerToken
    )

    http.POSTString(url, xml, customHeaders)
  }
}

trait MessageConnector {
  def post(xml: String, xMessageType: XMessageType, dateTime: OffsetDateTime)(implicit headerCarrier: HeaderCarrier): Future[HttpResponse]
}
