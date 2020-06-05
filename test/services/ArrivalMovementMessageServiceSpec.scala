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
import cats.data.NonEmptyList
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.MessageStatus
import models.MessageStatus.SubmissionPending
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import org.mockito.Mockito.when
import org.scalatest.concurrent.IntegrationPatience
import play.api.inject.bind
import repositories.ArrivalIdRepository
import utils.Format

import scala.concurrent.Future

class ArrivalMovementMessageServiceSpec extends SpecBase with IntegrationPatience {

  "makeArrivalMovement" - {
    "creates an arrival movement with an internal ref number and a mrn, date and time of creation from the message submitted with a message id of 1 and next correlation id of 2" in {
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)
      val dateTime   = LocalDateTime.of(dateOfPrep, timeOfPrep)

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

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

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
        status = ArrivalStatus.Initialized,
        dateTime,
        dateTime,
        messages = NonEmptyList.one(
          MovementMessageWithStatus(dateTime, MessageType.ArrivalNotification, movement, MessageStatus.SubmissionPending, 1)
        ),
        nextMessageCorrelationId = 2
      )

      service.makeArrivalMovement(eori)(movement).value.futureValue mustEqual expectedArrival
    }

    "returns None when the root node is not <CC007A>" in {

      val mrn        = MovementReferenceNumber("MRN")
      val eori       = "eoriNumber"
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

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

  "makeMessage" - {

    "when given a goodsReleasedMessage" - {

      "returns a Goods Released message" in {

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val application = baseApplicationBuilder.build()

        val service = application.injector.instanceOf[ArrivalMovementMessageService]

        val movement =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC025A>

        val messageCorrelationId = 1
        val expectedMessage      = MovementMessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.GoodsReleased, movement, messageCorrelationId)

        service.makeMessage(messageCorrelationId, MessageType.GoodsReleased)(movement).value mustEqual expectedMessage
      }

      "returns None when the root node is not <CC025A>" in {

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val application = baseApplicationBuilder.build()

        val service = application.injector.instanceOf[ArrivalMovementMessageService]

        val movement =
          <Foo>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </Foo>

        val messageCorrelationId = 1

        service.makeMessage(messageCorrelationId, MessageType.GoodsReleased)(movement) must not be defined
      }
    }

    "when given a unloadingPermissionMessage" - {

      "returns a Unloading Permission message" in {

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val application = baseApplicationBuilder.build()

        val service = application.injector.instanceOf[ArrivalMovementMessageService]

        val movement =
          <CC043A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC043A>

        val messageCorrelationId = 1
        val expectedMessage =
          MovementMessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.UnloadingPermission, movement, messageCorrelationId)

        service.makeMessage(messageCorrelationId, MessageType.UnloadingPermission)(movement).value mustEqual expectedMessage
      }

      "returns None when the root node is not <CC043A>" in {

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)

        val application = baseApplicationBuilder.build()

        val service = application.injector.instanceOf[ArrivalMovementMessageService]

        val movement =
          <Foo>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </Foo>

        val messageCorrelationId = 1

        service.makeMessage(messageCorrelationId, MessageType.UnloadingPermission)(movement) must not be defined
      }
    }

  }

  "makeMovementMessageWithState" - {

    "returns a message with the Unloading Remarks xml payload" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

      val movement =
        <CC044A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC044A>

      val messageCorrelationId = 1
      val expectedMessage =
        MovementMessageWithStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.UnloadingRemarks, movement, SubmissionPending, messageCorrelationId)

      service.makeMovementMessageWithStatus(messageCorrelationId, MessageType.UnloadingRemarks)(movement).value mustEqual expectedMessage
    }

    "does not return a message when the root node does not match the message type" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

      val movement =
        <Foo>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </Foo>

      service.makeMovementMessageWithStatus(1, MessageType.UnloadingRemarks)(movement) must not be defined
    }
  }

}
