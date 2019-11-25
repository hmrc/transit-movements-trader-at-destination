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

import config.AppConfig
import connectors.MessageConnector
import javax.inject.Inject
import models.Source
import models.messages.{ArrivalNotification, MessageCode}
import play.api.mvc.Result
import play.api.mvc.Results.NoContent
import repositories.ArrivalNotificationRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.Node
import scala.xml.Utility.trim

class XmlSubmissionServiceImpl @Inject()(
                                   messageConnector: MessageConnector,
                                   submissionModelService: SubmissionModelService,
                                   appConfig: AppConfig,
                                   xmlBuilderService: XmlBuilderService,
                                   xmlValidationService: XmlValidationService,
                                   arrivalNotificationRepository: ArrivalNotificationRepository
                                 ) extends XmlSubmissionService {

//  def buildAndValidateXml(
//              arrivalNotification: ArrivalNotification,
//              interchangeControllerReference: InterchangeControlReference
//            )(implicit hc: HeaderCarrier, ec: ExecutionContext): Either[RequestModelError, Node] = {
//
//    val messageSender = MessageSender(appConfig.env, "eori")
//
//    for {
//      request   <- submissionModelService.convertFromArrivalNotification(arrivalNotification, messageSender, interchangeControllerReference).right
//      xml       <- xmlBuilderService.buildXml(request)(dateTime = LocalDateTime.now()).right
//      _         <- xmlValidationService.validate(xml.toString(), ArrivalNotificationXSD).right
//    } yield xml
//  }

  def saveAndSubmitXml(xml: Node, messageCode: MessageCode, channel: Source, arrivalNotification: ArrivalNotification)
                      (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    for {
      _ <- messageConnector.post(trim(xml).toString(), messageCode, channel)
      _ <- arrivalNotificationRepository.persistToMongo(arrivalNotification)
    } yield NoContent
  }

}

trait XmlSubmissionService {
//  def buildAndValidateXml(arrivalNotification: ArrivalNotification, interchangeControllerReference: InterchangeControlReference)
//              (implicit hc: HeaderCarrier, ec: ExecutionContext): Either[RequestModelError, Node]

  def saveAndSubmitXml(xml: Node, messageCode: MessageCode, channel: Source, arrivalNotification: ArrivalNotification)
              (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Result]

  }
