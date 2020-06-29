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
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult._
import play.api.Logger
import repositories.ArrivalMovementRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class SaveMessageService @Inject()(arrivalMovementRepository: ArrivalMovementRepository,
                                   arrivalMovementService: ArrivalMovementMessageService,
                                   xmlValidationService: XmlValidationService)(implicit ec: ExecutionContext) {

  def validateXmlAndSaveMessage(messageXml: NodeSeq,
                                messageSender: MessageSender,
                                messageResponse: MessageResponse,
                                arrivalStatus: ArrivalStatus): Future[SubmissionProcessingResult] =
    xmlValidationService.validate(messageXml.toString(), messageResponse.xsdFile) match {
      case Success(_) =>
        println("*************")
        println(s"SHOULD NOT CALL THIS")

        arrivalMovementService.makeMessage(messageSender.messageCorrelationId, messageResponse.messageType)(messageXml) match {
          case Some(message) =>
            arrivalMovementRepository
              .addResponseMessage(messageSender.arrivalId, message, arrivalStatus)
              .map {
                case Success(_) => SubmissionSuccess
                case Failure(e) => {

                  println("*************")
                  println(s"internal failure $e")

                  SubmissionFailureInternal
                }
              }
          case None => Future.successful(SubmissionFailureExternal)
        }
      case Failure(e) => {
        Logger.warn(s"Failure to validate against XSD. Exception: ${e.getMessage}")
        Future.successful(SubmissionFailureExternal)
      }
    }
}
