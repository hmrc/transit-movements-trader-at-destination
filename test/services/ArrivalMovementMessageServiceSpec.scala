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
import models.MessageStatus.SubmissionPending
import models.Arrival
import models.ArrivalId
import models.ArrivalStatus
import models.MessageSender
import models.MessageStatus
import models.MessageType
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MovementReferenceNumber
import models.ParseError.InvalidRootNode
import org.mockito.Mockito.when
import org.scalatest.StreamlinedXmlEquality
import org.scalatest.concurrent.IntegrationPatience
import play.api.inject.bind
import repositories.ArrivalIdRepository
import utils.Format

import scala.concurrent.Future
import scala.xml.NodeSeq
import scala.xml.Utility.trim

class ArrivalMovementMessageServiceSpec extends SpecBase with IntegrationPatience with StreamlinedXmlEquality {

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
          <SynVerNumMES2>1</SynVerNumMES2>
          <HEAHEA>
            <DocNumHEA5>{mrn.value}</DocNumHEA5>
          </HEAHEA>
        </CC007A>.map(trim)

      val savedMovement =
        <CC007A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <SynVerNumMES2>1</SynVerNumMES2>
          <MesSenMES3>MDTP-000000000000000000000000001-01</MesSenMES3>
          <HEAHEA>
            <DocNumHEA5>{mrn.value}</DocNumHEA5>
          </HEAHEA>
        </CC007A>.map(trim)

      val expectedArrival = Arrival(
        arrivalId = id,
        movementReferenceNumber = mrn,
        eoriNumber = eori,
        status = ArrivalStatus.Initialized,
        dateTime,
        dateTime,
        dateTime,
        messages = NonEmptyList.one(
          MovementMessageWithStatus(dateTime, MessageType.ArrivalNotification, savedMovement, MessageStatus.SubmissionPending, 1)
        ),
        nextMessageCorrelationId = 2
      )

      service.makeArrivalMovement(eori, movement).futureValue.right.get mustEqual expectedArrival

      application.stop()
    }

    "returns InvalidRootNode when the root node is not <CC007A>" in {

      val id         = ArrivalId(1)
      val mrn        = MovementReferenceNumber("MRN")
      val eori       = "eoriNumber"
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val mockArrivalIdRepository = mock[ArrivalIdRepository]
      when(mockArrivalIdRepository.nextId()).thenReturn(Future.successful(id))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalIdRepository].toInstance(mockArrivalIdRepository)
        )
        .build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

      val invalidPayload =
        <Foo>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>{mrn.value}</DocNumHEA5>
          </HEAHEA>
        </Foo>

      service.makeArrivalMovement(eori, invalidPayload).futureValue.left.get mustBe an[InvalidRootNode]

      application.stop()
    }
  }

  "makeInboundMessage" - {

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

        service.makeInboundMessage(messageCorrelationId, MessageType.GoodsReleased)(movement).right.get mustEqual expectedMessage
        application.stop()
      }

      "returns InvalidRootNode when the root node is not <CC025A>" in {

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

        service.makeInboundMessage(messageCorrelationId, MessageType.GoodsReleased)(movement).left.get mustBe an[InvalidRootNode]
        application.stop()
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

        service.makeInboundMessage(messageCorrelationId, MessageType.UnloadingPermission)(movement).right.get mustEqual expectedMessage
        application.stop()
      }

      "returns InvalidRootNode when the root node is not <CC043A>" in {

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

        service.makeInboundMessage(messageCorrelationId, MessageType.UnloadingPermission)(movement).left.get mustBe an[InvalidRootNode]
        application.stop()
      }
    }

  }

  "makeOutboundMessage" - {

    "returns a message with the correct MesSenMES3 in xml payload" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)
      val id         = ArrivalId(1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

      val movement =
        <CC044A><DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9><TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10><SynVerNumMES2>test</SynVerNumMES2></CC044A>

      val expectedMovement: NodeSeq =
        <CC044A><DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9><TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10><SynVerNumMES2>test</SynVerNumMES2><MesSenMES3>MDTP-000000000000000000000000001-01</MesSenMES3></CC044A>

      val messageCorrelationId = 1
      val expectedMessage =
        MovementMessageWithStatus(LocalDateTime.of(dateOfPrep, timeOfPrep),
                                  MessageType.UnloadingRemarks,
                                  expectedMovement.map(trim),
                                  SubmissionPending,
                                  messageCorrelationId)

      val result: MovementMessage =
        service.makeOutboundMessage(id, messageCorrelationId, MessageType.UnloadingRemarks)(movement.map(trim)).right.get

      result mustEqual expectedMessage
      application.stop()
    }

    "returns a message with the Unloading Remarks xml payload" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)
      val id         = ArrivalId(1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

      val movement =
        <CC044A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <SynVerNumMES2>1</SynVerNumMES2>
        </CC044A>.map(trim)

      val expectedMovement =
        <CC044A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <SynVerNumMES2>1</SynVerNumMES2>
          <MesSenMES3>{MessageSender(id, 1).toString}</MesSenMES3>
        </CC044A>.map(trim)

      val messageCorrelationId = 1
      val expectedMessage =
        MovementMessageWithStatus(LocalDateTime.of(dateOfPrep, timeOfPrep),
                                  MessageType.UnloadingRemarks,
                                  expectedMovement,
                                  SubmissionPending,
                                  messageCorrelationId)

      service.makeOutboundMessage(id, messageCorrelationId, MessageType.UnloadingRemarks)(movement).right.get mustEqual expectedMessage
      application.stop()
    }

    "returns InvalidRootNode when the root node does not match the message type" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)
      val id         = ArrivalId(1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[ArrivalMovementMessageService]

      val movement =
        <Foo>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </Foo>

      service.makeOutboundMessage(id, 1, MessageType.UnloadingRemarks)(movement).left.get mustBe an[InvalidRootNode]
      application.stop()
    }
  }

}
