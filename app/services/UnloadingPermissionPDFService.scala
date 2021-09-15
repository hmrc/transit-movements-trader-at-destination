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

import connectors.ManageDocumentsConnector
import controllers.Assets.CONTENT_DISPOSITION
import controllers.Assets.CONTENT_TYPE
import models.Arrival
import models.WSError
import models.WSError._
import play.api.mvc.Results
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class UnloadingPermissionPDFService @Inject()(messageRetrievalService: MessageRetrievalService, manageDocumentsConnector: ManageDocumentsConnector)
    extends Results {

  def getPDF(arrival: Arrival)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[WSError, (Array[Byte], Seq[(String, String)])]] =
    messageRetrievalService.getUnloadingPermission(arrival) match {
      case Some(unloadingPermission) =>
        manageDocumentsConnector.getUnloadingPermissionPdf(unloadingPermission).map {
          result =>
            result.status match {
              case 200 =>
                val contentDisposition = result.headers.get(CONTENT_DISPOSITION).map(value => Seq((CONTENT_DISPOSITION, value.head))).getOrElse(Seq.empty)
                val contentType        = result.headers.get(CONTENT_TYPE).map(value => Seq((CONTENT_TYPE, value.head))).getOrElse(Seq.empty)

                Right((result.bodyAsBytes.toArray, contentDisposition ++ contentType))
              case otherErrorCode => Left(OtherError(otherErrorCode, result.body))
            }
        }
      case None => Future.successful(Left(NotFoundError))
    }

}
