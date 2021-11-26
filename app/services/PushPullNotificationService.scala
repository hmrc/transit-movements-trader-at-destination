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

package services

import connectors.PushPullNotificationConnector
import models.ArrivalMessageNotification
import models.Box
import models.InboundMessageRequest
import play.api.Logging
import play.api.http.HeaderNames
import play.api.http.Status._
import play.api.mvc.Headers
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.UpstreamErrorResponse

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import scala.util.control.NonFatal

class PushPullNotificationService @Inject()(connector: PushPullNotificationConnector)(implicit ec: ExecutionContext) extends Logging {

  def getBox(clientId: String)(implicit hc: HeaderCarrier): Future[Option[Box]] =
    connector
      .getBox(clientId)
      .map {
        case Left(UpstreamErrorResponse(message, statusCode, _, _)) =>
          if (statusCode != NOT_FOUND) logger.warn(s"Unable to retrieve box id from PPNS. Response from PPNS: Error $statusCode $message")
          None
        case Right(box) => Some(box)
      }
      .recover {
        case NonFatal(e) =>
          logger.error("Unable to retrieve box id from PPNS", e)
          None
      }

  private[services] def sendPushNotification(inboundRequest: InboundMessageRequest, headers: Headers)(implicit hc: HeaderCarrier): Future[Unit] =
    inboundRequest.arrival.notificationBox
      .map {
        box =>
          val contentLength = headers
            .get(HeaderNames.CONTENT_LENGTH)
            .flatMap(
              x => Try(x.toInt).toOption
            )

          val arrivalMessageNotification = ArrivalMessageNotification.fromInboundRequest(inboundRequest, contentLength)

          connector
            .postNotification(box.boxId, arrivalMessageNotification)
            .map {
              case Left(UpstreamErrorResponse(message, statusCode, _, _)) =>
                logger.warn(s"Unable to post message to PPNS. Response from PPNS: Error $statusCode $message")
              case Right(_) => ()

            }
            .recover {
              case NonFatal(e) =>
                logger.error("Unable to post message to PPNS", e)
            }
      }
      .getOrElse(Future.unit)
}
