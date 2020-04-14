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
import java.time.LocalDateTime
import java.time.LocalTime

import base.SpecBase
import models.Arrival
import models.MessageType
import models.MovementMessage
import models.MovementReferenceNumber
import models.State
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
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val id   = ArrivalId(1)
      val mrn  = MovementReferenceNumber("MRN")
      val eori = "eoriNumber"

      val mockArrivalIdRepository = mock[ArrivalIdRepository]
      when(mockArrivalIdRepository.nextId).thenReturn(Future.successful(id))
      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository)
        )
        .build()

      val service = application.injector.instanceOf[ArrivalMovementService]

      val movement =
        <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn.value}</DocNumHEA5>
            </HEAHEA>
          </CC007A>

      val expectedArrival = Arrival(
        arrivalId = id,
        movementReferenceNumber = mrn,
        eoriNumber = eori,
        state = State.PendingSubmission,
        LocalDateTime.of(dateOfPrep, timeOfPrep),
        LocalDateTime.of(dateOfPrep, timeOfPrep),
        messages = Seq(
          MovementMessage(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.ArrivalNotification, movement)
        )
      )

      service.makeArrivalMovement(eori)(movement).value.futureValue mustEqual expectedArrival
    }

    "returns None when the root node is not <CC007A>" in {

      val id         = ArrivalId(1)
      val mrn        = MovementReferenceNumber("MRN")
      val eori       = "eoriNumber"
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementService]

      val invalidPayload =
        <Foo>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>{mrn.value}</DocNumHEA5>
          </HEAHEA>
        </Foo>

      service.makeArrivalMovement(eori)(invalidPayload) must not be defined
    }
  }

  "makeGoodsReleasedMessage" - {

    "returns a Goods Released message" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementService]

      val movement =
        <CC025A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC025A>

      val expectedMessage = MovementMessage(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, movement)

      service.makeGoodsReleasedMessage()(movement).value mustEqual expectedMessage
    }

    "returns None when the root node is not <CC025A>" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementService]

      val movement =
        <Foo>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </Foo>

      val expectedMessage = MovementMessage(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, movement)

      service.makeGoodsReleasedMessage()(movement) must not be defined
    }
  }

  "dateOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          </CC007A>

      ArrivalMovementService.dateOfPrepR(movement).value mustEqual dateOfPrep

    }

    "will return a None when the date in the DatOfPreMES9 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep) ++ "1"}</DatOfPreMES9>
          </CC007A>

      ArrivalMovementService.dateOfPrepR(movement) must not be (defined)

    }

    "will return a None when the date in the DatOfPreMES9 node is missing" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC007A>
          </CC007A>

      ArrivalMovementService.dateOfPrepR(movement) must not be (defined)

    }

  }

  "timeOfPrepR" - {
    "returns the time from the TimOfPreMES10 node" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC007A>

      ArrivalMovementService.timeOfPrepR(movement).value mustEqual timeOfPrep

    }

    "returns a None if TimOfPreMES10 is malformed" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "a"}</TimOfPreMES10>
        </CC007A>

      ArrivalMovementService.timeOfPrepR(movement) must not be (defined)

    }

    "returns a None if TimOfPreMES10 is missing" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
        </CC007A>

      ArrivalMovementService.timeOfPrepR(movement) must not be (defined)

    }

  }

  "mrnR" - {
    "returns the mrn from the DocNumHEA5 node" in {
      val mrn = MovementReferenceNumber("MRN")

      val movement =
        <CC007A>
          <HEAHEA>
            <DocNumHEA5>{mrn.value}</DocNumHEA5>
          </HEAHEA>
        </CC007A>

      ArrivalMovementService.mrnR(movement).value mustEqual mrn

    }

    "returns None if DocNumHEA5 node is missing" in {
      val mrn = MovementReferenceNumber("MRN")

      val movement =
        <CC007A>
          <HEAHEA>
          </HEAHEA>
        </CC007A>

      ArrivalMovementService.mrnR(movement) must not be (defined)

    }

  }

  "correctRootNodeR" - {
    "returns true if the root node is as expected" in {

      val movement =
        <CC007A></CC007A>

      ArrivalMovementService.correctRootNodeR(MessageType.ArrivalNotification)(movement) mustBe (defined)
    }

    "returns false if the root node is not as expected" in {

      val movement =
        <Foo></Foo>

      ArrivalMovementService.correctRootNodeR(MessageType.ArrivalNotification)(movement) must not be defined
    }
  }

}
