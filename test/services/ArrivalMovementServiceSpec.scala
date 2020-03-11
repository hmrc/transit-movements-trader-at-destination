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

import java.time.LocalDate
import java.time.LocalTime

import base.SpecBase
import models.ArrivalMovement
import models.TimeStampedMessageXml
import models.request.InternalReferenceId
import org.mockito.Mockito.when
import org.scalatest.EitherValues
import play.api.inject.bind

import scala.concurrent.Future
import scala.xml.NodeSeq

class ArrivalMovementServiceSpec extends SpecBase with EitherValues {

  "makeArrivalMovement" - {
    "adds internal reference number and movement reference number" in {

      val ir   = InternalReferenceId(1)
      val mrn  = "MRN"
      val eori = "eoriNumber"

      val mockDatabaseService = mock[DatabaseService]
      when(mockDatabaseService.getInternalReferenceId).thenReturn(Future.successful(Right(ir)))
      val application = applicationBuilder
        .overrides(
          bind[DatabaseService].toInstance(mockDatabaseService)
        )
        .build()

      val service = application.injector.instanceOf[ArrivalMovementService]

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>19000101</DatOfPreMES9>
            <TimOfPreMES10>1231</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn}</DocNumHEA5>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      val msg = TimeStampedMessageXml(LocalDate.now(), LocalTime.now(), movement)

      val expectedArrivalMovement: ArrivalMovement = ArrivalMovement(
        internalReferenceId = ir.index,
        movementReferenceNumber = mrn,
        eoriNumber = eori,
        messages = Seq(
          msg
        )
      )

      service.makeArrivalMovement(msg, eori).futureValue.right.value mustEqual expectedArrivalMovement

      app.stop()
    }
  }
}
