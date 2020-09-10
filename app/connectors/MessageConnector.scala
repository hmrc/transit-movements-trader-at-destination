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
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult._
import models.ArrivalId
import models.MessageSender
import models.MessageType
import models.MovementMessageWithStatus
import models.TransitWrapper
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.logging.Authorization
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import utils.Format
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MessageConnector @Inject()(config: AppConfig, http: HttpClient)(implicit ec: ExecutionContext) {

  def post(arrivalId: ArrivalId, message: MovementMessageWithStatus, dateTime: OffsetDateTime)(
    implicit headerCarrier: HeaderCarrier
  ): Future[EisSubmissionResult] = {

    val xmlMessage = TransitWrapper(message.message).toString

    val url = config.eisUrl

    lazy val messageSender = MessageSender(arrivalId, message.messageCorrelationId)

    val newHeaders = headerCarrier
      .copy(authorization = Some(Authorization(s"Bearer ${config.eisBearerToken}")))
      .withExtraHeaders(addHeaders(message.messageType, dateTime, messageSender): _*)

    val possibleResponses           = List(EisSubmissionSuccessful, ErrorInPayload, VirusFoundOrInvalidToken, DownstreamInternalServerError)
    val statusesOfPossibleResponses = possibleResponses.map(_.httpStatus)
    val statusToResponseMapping     = statusesOfPossibleResponses.zip(possibleResponses).toMap

    http
      .POSTString[HttpResponse](url, xmlMessage)(readRaw, hc = newHeaders, implicitly)
      .map(response => statusToResponseMapping.getOrElse(response.status, UnexpectedHttpResponse(response)))
  }

  private def addHeaders(messageType: MessageType, dateTime: OffsetDateTime, messageSender: MessageSender)(
    implicit headerCarrier: HeaderCarrier): Seq[(String, String)] =
    Seq(
      "X-Forwarded-Host" -> "mdtp",
      "X-Correlation-ID" -> {
        headerCarrier.sessionId
          .map(x => removePrefix(sessionPrefix, x))
          .getOrElse(UUID.randomUUID().toString)
      },
      "Date"             -> Format.dateFormattedForHeader(dateTime),
      "Content-Type"     -> "application/xml",
      "Accept"           -> "application/xml",
      "X-Message-Type"   -> messageType.toString,
      "X-Message-Sender" -> messageSender.toString
    )

  private val sessionPrefix = "session-"

  private[connectors] def removePrefix(prefix: String, sessionId: SessionId): String =
    sessionId.value.replaceFirst(prefix, "")
}

object MessageConnector {

  sealed abstract class EisSubmissionResult(val httpStatus: Int, asString: String) {
    override def toString: String = s"EisSubmissionResult(code = $httpStatus, and details = " + asString + ")"
  }

  object EisSubmissionResult {
    object EisSubmissionSuccessful extends EisSubmissionResult(202, "EIS Successful Submission")

    sealed abstract class EisSubmissionFailure(httpStatus: Int, asString: String) extends EisSubmissionResult(httpStatus, asString)

    sealed abstract class EisSubmissionRejected(httpStatus: Int, asString: String) extends EisSubmissionFailure(httpStatus, asString)
    object ErrorInPayload                                                          extends EisSubmissionRejected(400, "Message failed schema validation")
    object VirusFoundOrInvalidToken                                                extends EisSubmissionRejected(403, "Virus found, token invalid etc")

    sealed abstract class EisSubmissionFailureDownstream(httpStatus: Int, asString: String) extends EisSubmissionFailure(httpStatus, asString)
    object DownstreamInternalServerError                                                    extends EisSubmissionFailureDownstream(500, "Downstream internal server error")
    case class UnexpectedHttpResponse(httpResponse: HttpResponse)
        extends EisSubmissionFailureDownstream(httpResponse.status, "Unexpected HTTP Response received")
  }
}
