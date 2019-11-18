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

package services

import java.time.LocalDateTime

import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import config.AppConfig
import connectors.MessageConnector
import javax.inject.Inject
import models.messages.ArrivalNotification
import models.messages.request.{InterchangeControlReference, MessageSender, RequestModelError}
import models.{ArrivalNotificationXSD, WebChannel}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

class SubmissionServiceImpl @Inject()(
                                   messageConnector: MessageConnector,
                                   submissionModelService: SubmissionModelService,
                                   appConfig: AppConfig,
                                   xmlBuilderService: XmlBuilderService,
                                   xmlValidationService: XmlValidationService
                                 ) extends SubmissionService {

  def submit(arrivalNotification: ArrivalNotification)
            (implicit hc: HeaderCarrier, ec: ExecutionContext): Either[RequestModelError, Future[HttpResponse]] = {

    val messageSender = MessageSender(appConfig.env, "eori")
    val interchangeControllerReference = InterchangeControlReference("11122017", 1)

    for {
      request   <- submissionModelService.convertFromArrivalNotification(arrivalNotification, messageSender, interchangeControllerReference).right
      xml       <- xmlBuilderService.buildXml(request)(dateTime = LocalDateTime.now()).right
      _         <- xmlValidationService.validate(xml.toString(), ArrivalNotificationXSD).right
    } yield {
      messageConnector.post(xml.toString, request.messageCode, WebChannel)
    }
  }

}

trait SubmissionService {
  def submit(arrivalNotification: ArrivalNotification)
            (implicit hc: HeaderCarrier, ec: ExecutionContext): Either[RequestModelError, Future[HttpResponse]]

}
