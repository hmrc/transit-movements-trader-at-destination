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

import cats.implicits._
import connectors.ManageDocumentsConnector
import javax.inject.Inject
import models.Arrival
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class UnloadingPermissionPDFService @Inject()(
  messageRetrievalService: MessageRetrievalService,
  manageDocumentsConnector: ManageDocumentsConnector
) {

  def getPDF(arrival: Arrival)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Array[Byte]]] =
    messageRetrievalService.getUnloadingPermission(arrival).traverse {
      unloadingPermission =>
        manageDocumentsConnector.getUnloadingPermissionPdf(unloadingPermission)
    }

}
