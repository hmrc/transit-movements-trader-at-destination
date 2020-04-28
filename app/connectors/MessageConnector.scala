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
import models.ArrivalId
import models.MessageSender
import models.MessageType
import models.MovementMessageWithState
import models.TransitWrapper
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.logging.Authorization
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import utils.Format

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MessageConnector @Inject()(config: AppConfig, http: HttpClient)(implicit ec: ExecutionContext) {

  def post(arrivalId: ArrivalId, message: MovementMessageWithState, dateTime: OffsetDateTime)(
    implicit headerCarrier: HeaderCarrier
  ): Future[HttpResponse] = {

    val xmlMessage = TransitWrapper(message.message).toString

    val url = config.eisUrl

    lazy val messageSender = MessageSender(arrivalId, message.messageCorrelationId)

    val newHeaders = headerCarrier
      .copy(authorization = Some(Authorization("Bearer securityToken")))
      .withExtraHeaders(addHeaders(message.messageType, dateTime, messageSender): _*)

    // TODO: Don't throw exceptions here
    http.POSTString(url, xmlMessage)(rds = HttpReads.readRaw, hc = newHeaders, ec = ec)
  }

  private def addHeaders(messageType: MessageType, dateTime: OffsetDateTime, messageSender: MessageSender)(
    implicit headerCarrier: HeaderCarrier): Seq[(String, String)] =
    Seq(
      "X-Forwarded-Host" -> "mdtp",
      "X-Correlation-ID" -> {
        headerCarrier.sessionId
          .map(_.value)
          .getOrElse(UUID.randomUUID().toString)
      },
      "Date"             -> Format.dateFormattedForHeader(dateTime),
      "Content-Type"     -> "application/xml",
      "Accept"           -> "application/xml",
      "X-Message-Type"   -> messageType.toString,
      "X-Message-Sender" -> messageSender.toString
    )
}
