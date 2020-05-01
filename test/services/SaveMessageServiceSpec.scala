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
import models.ArrivalId
import models.ArrivalStatus._
import models.GoodsReleasedResponse
import models.MessageSender
import models.SubmissionResult
import repositories.ArrivalMovementRepository
import utils.Format
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito._
import play.api.inject.bind

import scala.concurrent.Future
import scala.util.Success

class SaveMessageServiceSpec extends SpecBase {

  "asdf" - {
    "Returns Success when we successfully save a message" in {
      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      when(mockArrivalMovementRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val arrivalId     = ArrivalId(1)
      val version       = 1
      val messageSender = MessageSender(arrivalId, version)

      val requestGoodsReleasedXmlBody =
        <CC025A>
        <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
      </CC025A>

      val result = saveMessageService.asdf(requestGoodsReleasedXmlBody, messageSender, GoodsReleasedResponse, GoodsReleased).futureValue

      result mustBe SubmissionResult.Success
      verify(mockArrivalMovementRepository, times(1)).addResponseMessage(eqTo(arrivalId), any(), eqTo(GoodsReleased))
    }

  }
}
