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

import base.SpecBase
import cats.data.ReaderT
import generators.ModelGenerators
import models.MessageType.GoodsReleased
import models.FailedToMakeMovementMessage
import models.MovementMessageWithoutStatus
import models.ParseError.InvalidRootNode
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running
import services.XmlMessageParser.ParseHandler

import scala.xml.NodeSeq

class MovementMessageServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockArrivalMovementMessageService = mock[ArrivalMovementMessageService]

  "MovementMessageService" - {

    "makeMovementMessage" - {

      "must return a MovementMessage" in {

        val movementMessage = arbitrary[MovementMessageWithoutStatus].sample.value

        val expectedReaderT = ReaderT[ParseHandler, NodeSeq, MovementMessageWithoutStatus] {
          _ =>
            Right(movementMessage)
        }

        when(mockArrivalMovementMessageService.makeInboundMessage(any(), any(), any()))
          .thenReturn(expectedReaderT)

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementMessageService].toInstance(mockArrivalMovementMessageService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[MovementMessageService]

          val result = service.makeMovementMessage(0, GoodsReleased, NodeSeq.Empty).value

          result.futureValue.value mustBe movementMessage
        }
      }

      "must return a FailedToMakeMovementMessage when a movement message cannot be created" in {

        val expectedReaderT = ReaderT[ParseHandler, NodeSeq, MovementMessageWithoutStatus] {
          _ =>
            Left(InvalidRootNode("error"))
        }

        when(mockArrivalMovementMessageService.makeInboundMessage(any(), any(), any()))
          .thenReturn(expectedReaderT)

        val application = baseApplicationBuilder
          .overrides(bind[ArrivalMovementMessageService].toInstance(mockArrivalMovementMessageService))
          .build()

        running(application) {
          val service = application.injector.instanceOf[MovementMessageService]

          val result = service.makeMovementMessage(0, GoodsReleased, NodeSeq.Empty).value

          result.futureValue.left.value mustBe an[FailedToMakeMovementMessage]
        }
      }
    }
  }

}
