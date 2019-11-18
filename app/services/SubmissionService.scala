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

import config.AppConfig
import connectors.MessageConnector
import javax.inject.Inject
import models.messages.ArrivalNotification
import models.messages.request.{ArrivalNotificationRequest, InterchangeControlReference, MessageSender, RequestModelError}
import models.{ArrivalNotificationXSD, WebChannel}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class SubmissionService @Inject()(
                                   messageConnector: MessageConnector,
                                   submissionModelService: SubmissionModelService,
                                   appConfig: AppConfig,
                                   xmlBuilderService: XmlBuilderService,
                                   xmlValidationService: XmlValidationService
                                 ) {

  def submit(arrivalNotification: ArrivalNotification)
            (implicit hc: HeaderCarrier, ec: ExecutionContext): Try[Future[HttpResponse]] = {

    val messageSender = MessageSender(appConfig.env, "eori")
    val interchangeControllerReference = InterchangeControlReference("11122017", 1)

    val request: Either[RequestModelError, ArrivalNotificationRequest] = {
      submissionModelService.convertFromArrivalNotification(arrivalNotification, messageSender, interchangeControllerReference)
    }

    val builder = xmlBuilderService.buildXml(request.right.get)(dateTime = LocalDateTime.now())

    val xmlValidation: Try[String] = xmlValidationService.validate(builder.right.get.toString(), ArrivalNotificationXSD)

    xmlValidation.map {
      xml =>
        messageConnector.post(xml, request.right.get.messageCode, WebChannel)
    }
  }

}
