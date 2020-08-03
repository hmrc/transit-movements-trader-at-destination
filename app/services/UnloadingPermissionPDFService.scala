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

import connectors.ManageDocumentsConnector
import javax.inject.Inject
import models.WSError._
import models.Arrival
import models.WSError
import play.api.mvc.Results
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class UnloadingPermissionPDFService @Inject()(messageRetrievalService: MessageRetrievalService, manageDocumentsConnector: ManageDocumentsConnector)
    extends Results {

  def getPDF(arrival: Arrival)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[WSError, Array[Byte]]] =
    messageRetrievalService.getUnloadingPermission(arrival) match {
      case Some(unloadingPermission) =>
        manageDocumentsConnector.getUnloadingPermissionPdf(unloadingPermission).map {
          result =>
            result.status match {
              case 200            => Right(result.bodyAsBytes.toArray)
              case otherErrorCode => Left(OtherError(otherErrorCode, result.body))
            }
        }
      case None => Future.successful(Left(NotFoundError))
    }

}
