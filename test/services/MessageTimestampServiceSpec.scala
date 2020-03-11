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
import models.TimeStampedMessageXml
import utils.Format

class MessageTimestampServiceSpec extends SpecBase {

  "makeMovement" - {
    "return the message with the date and time of message preparation" in {
      val application = app
      val service     = application.injector.instanceOf[MessageTimestampService]

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      val expectedMessage = TimeStampedMessageXml(dateOfPrep, timeOfPrep, movement)

      service.deriveTimestamp(movement).value mustEqual expectedMessage

      app.stop()
    }

    "return a none when date of preparation is malformed" in {
      val application = app
      val service     = application.injector.instanceOf[MessageTimestampService]

      val timeOfPrep = LocalTime.of(1, 1)

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>198801211</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      service.deriveTimestamp(movement) must not be (defined)

      app.stop()
    }

    "return a none when time of preparation is malformed" in {
      val application = app
      val service     = application.injector.instanceOf[MessageTimestampService]

      val dateOfPrep = LocalDate.now()

      val movement =
        <transitRequest>
          <CC007A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>12311</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC007A>
        </transitRequest>

      service.deriveTimestamp(movement) must not be (defined)

      app.stop()
    }

  }

}
