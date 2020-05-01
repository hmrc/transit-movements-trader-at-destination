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
import models.GoodsReleasedResponse
import models.MessageSender
import models.SubmissionResult.Success
import org.scalatest.FreeSpec
import utils.Format

class SaveMessageServiceSpec extends SpecBase {

  "Methods tests" - {
    "Returns success for valid input of message" in {

      val saveMessageService = new SaveMessageService

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

      val result = saveMessageService.asdf(requestGoodsReleasedXmlBody, messageSender, GoodsReleasedResponse).futureValue

      result mustBe Success
    }

  }
}
