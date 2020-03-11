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
import models.ArrivalMovement
import models.TimeStampedMessageXml
import repositories.FailedCreatingNextInternalReferenceId

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ArrivalMovementService @Inject()(databaseService: DatabaseService)(implicit ec: ExecutionContext) {

  def makeArrivalMovement(thing: TimeStampedMessageXml, eori: String): Future[Either[FailedCreatingNextInternalReferenceId, ArrivalMovement]] = {
    val mrn = (thing.body \ "CC007A" \ "HEAHEA" \ "DocNumHEA5").text

    databaseService.getInternalReferenceId.map(
      _.right
        .map(_.index)
        .right
        .map(ArrivalMovement(_, mrn, eori, Seq(thing)))
    )
  }

}
