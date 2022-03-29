/*
 * Copyright 2022 HM Revenue & Customs
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

import connectors.ManageDocumentsConnector
import models.Arrival
import models.MovementMessage
import models.PdfDocument
import models.WSError
import models.WSError._
import play.api.mvc.Results
import play.api.http.Status._
import play.api.http.HeaderNames._
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class UnloadingPermissionPDFService @Inject() (messageRetrievalService: MessageRetrievalService, manageDocumentsConnector: ManageDocumentsConnector)
    extends Results {

  val explicitHeaders = Set(CONTENT_LENGTH, CONTENT_TYPE, CONTENT_DISPOSITION)

  private def generatePDF(messageBody: NodeSeq)(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    manageDocumentsConnector.getUnloadingPermissionPdf(messageBody).map {
      result =>
        result.status match {
          case OK =>
            Right(
              PdfDocument(
                result.bodyAsSource,
                result.header(CONTENT_LENGTH).map(_.toLong),
                result.header(CONTENT_TYPE),
                result.header(CONTENT_DISPOSITION)
              )
            )
          case otherErrorCode =>
            Left(OtherError(otherErrorCode, result.body))
        }
    }

  def getPDF(arrival: Arrival)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[WSError, PdfDocument]] =
    messageRetrievalService.getUnloadingPermission(arrival) match {
      case Some(unloadingPermission) => generatePDF(unloadingPermission.message)
      case None                      => Future.successful(Left(NotFoundError))
    }

  def getPDF(messages: List[MovementMessage])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[WSError, PdfDocument]] =
    generatePDF(messages.minBy(_.messageCorrelationId).message)

}
