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
import models.Arrival
import models.TimeStampedMessageXml
import models.messages.MovementReferenceNumber
import models.request.ArrivalId
import org.mockito.Mockito.when
import org.scalatest.concurrent.IntegrationPatience
import play.api.inject.bind
import repositories.ArrivalIdRepository
import utils.Format

import scala.concurrent.Future

class ArrivalMovementServiceSpec extends SpecBase with IntegrationPatience {

  "makeArrivalMovement" - {
    "creates an arrival movement with an internal ref number and a mrn, date and time of creation from the message submitted" in {

      val id         = ArrivalId(1)
      val mrn        = "MRN"
      val eori       = "eoriNumber"
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val mockArrivalIdRepository = mock[ArrivalIdRepository]
      when(mockArrivalIdRepository.nextId).thenReturn(Future.successful(id))
      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository)
        )
        .build()

      val service = application.injector.instanceOf[ArrivalMovementService]

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn}</DocNumHEA5>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      val expectedArrival = Arrival(
        arrivalId = id,
        movementReferenceNumber = mrn,
        eoriNumber = eori,
        messages = Seq(
          TimeStampedMessageXml(dateOfPrep, timeOfPrep, movement)
        )
      )

      service.makeArrivalMovement(eori)(movement).value.futureValue mustEqual expectedArrival
    }
  }

  "dateOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.dateOfPrepR(movement).value mustEqual dateOfPrep

    }

    "will return a None when the date in the DatOfPreMES9 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep) ++ "1"}</DatOfPreMES9>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.dateOfPrepR(movement) must not be (defined)

    }

    "will return a None when the date in the DatOfPreMES9 node is missing" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <transitRequest>
          <CC007A>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.dateOfPrepR(movement) must not be (defined)

    }

  }

  "timeOfPrepR" - {
    "returns the time from the TimOfPreMES10 node" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <transitRequest>
          <CC007A>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.timeOfPrepR(movement).value mustEqual timeOfPrep

    }

    "returns a None if TimOfPreMES10 is malformed" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <transitRequest>
          <CC007A>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "a"}</TimOfPreMES10>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.timeOfPrepR(movement) must not be (defined)

    }

    "returns a None if TimOfPreMES10 is missing" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <transitRequest>
          <CC007A>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.timeOfPrepR(movement) must not be (defined)

    }

  }

  "mrnR" - {
    "returns the mrn from the DocNumHEA5 node" in {
      val mrn = MovementReferenceNumber("MRN")

      val movement =
        <transitRequest>
          <CC007A>
            <HEAHEA>
              <DocNumHEA5>{mrn.value}</DocNumHEA5>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.mrnR(movement).value mustEqual mrn

    }

    "returns None if DocNumHEA5 node is missing" in {
      val mrn = MovementReferenceNumber("MRN")

      val movement =
        <transitRequest>
          <CC007A>
            <HEAHEA>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      ArrivalMovementService.mrnR(movement) must not be (defined)

    }

  }

}