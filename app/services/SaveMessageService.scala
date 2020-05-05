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

package services

import com.google.inject.Inject
import models.ArrivalStatus
import models.MessageResponse
import models.MessageSender
import models.SubmissionResult
import play.api.Logger
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class SaveMessageService @Inject()(arrivalMovementRepository: ArrivalMovementRepository,
                                   arrivalMovementService: ArrivalMovementService,
                                   xmlValidationService: XmlValidationService)(implicit ec: ExecutionContext) {

  private val logger = Logger(getClass)

  def validateXmlAndSaveMessage(messageXml: NodeSeq,
                                messageSender: MessageSender,
                                messageResponse: MessageResponse,
                                arrivalStatus: ArrivalStatus): Future[SubmissionResult] =
    xmlValidationService.validate(messageXml.toString(), messageResponse.xsdFile) match {
      case Success(_) =>
        arrivalMovementService.makeMessage(messageSender.messageCorrelationId, messageResponse.messageType)(messageXml) match {
          case Some(message) =>
            arrivalMovementRepository
              .addResponseMessage(messageSender.arrivalId, message, arrivalStatus)
              .map {
                case Success(_) => SubmissionResult.Success
                case Failure(_) => SubmissionResult.FailureInternal
              }
          case None => Future.successful(SubmissionResult.FailureExternal)
        }
      case Failure(e) => {
        logger.error(s"Failure to validate against XSD. Exception: ${e.getMessage}")
        Future.successful(SubmissionResult.FailureExternal)
      }
    }
}
