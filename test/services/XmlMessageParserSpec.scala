/*
 * Copyright 2022 HM Revenue & Customs
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
import models.MessageType
import models.MovementReferenceNumber
import models.ParseError._
import utils.Format

import scala.xml.NodeSeq

class XmlMessageParserSpec extends SpecBase {

  "dateOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC007A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        </CC007A>

      XmlMessageParser.dateOfPrepR(movement).right.get mustEqual dateOfPrep

    }

    "will return a LocalDateParseFailure when the date is longer than 8 with a specific message" in {
      val movement =
        <CC007A>
          <DatOfPreMES9>202105051</DatOfPreMES9>
        </CC007A>

      val result = XmlMessageParser.dateOfPrepR(movement).left.get
      result mustBe an[LocalDateParseFailure]
      result.message mustBe "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
    }

    "will return a LocalDateParseFailure when the date is shorter than 6" in {
      val movement =
        <CC007A>
          <DatOfPreMES9>20201</DatOfPreMES9>
        </CC007A>

      val result = XmlMessageParser.dateOfPrepR(movement).left.get
      result mustBe an[LocalDateParseFailure]
      result.message mustBe "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
    }

    "will return a LocalDateParseFailure when the date is 7 digits" in {
      val movement =
        <CC007A>
          <DatOfPreMES9>2020121</DatOfPreMES9>
        </CC007A>

      val result = XmlMessageParser.dateOfPrepR(movement).left.get
      result mustBe an[LocalDateParseFailure]
      result.message mustBe "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
    }

    "will return a LocalDateParseFailure when the date in the DatOfPreMES9 node is missing" in {
      val movement =
        <CC007A>
        </CC007A>

      XmlMessageParser.dateOfPrepR(movement).left.get mustBe an[LocalDateParseFailure]
    }

  }

  "timeOfPrepR" - {
    "returns the time from the TimOfPreMES10 node" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC007A>

      XmlMessageParser.timeOfPrepR(movement).right.get mustEqual timeOfPrep

    }

    "returns a LocalTimeParseFailure if TimOfPreMES10 is malformed" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "a"}</TimOfPreMES10>
        </CC007A>

      XmlMessageParser.timeOfPrepR(movement).left.get mustBe an[LocalTimeParseFailure]

    }

    "returns a LocalTimeParseFailure if TimOfPreMES10 is missing" in {
      val movement =
        <CC007A>
        </CC007A>

      XmlMessageParser.timeOfPrepR(movement).left.get mustBe an[LocalTimeParseFailure]

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

      XmlMessageParser.mrnR(movement).right.get mustEqual mrn

    }

    "returns EmptyMovementReferenceNumber if DocNumHEA5 node is missing" in {
      val movement =
        <CC007A>
          <HEAHEA>
          </HEAHEA>
        </CC007A>

      XmlMessageParser.mrnR(movement).left.get mustBe an[EmptyMovementReferenceNumber]
    }

    "returns EmptyMovementReferenceNumber if given a NodeSeq.Empty" in {
      XmlMessageParser.mrnR(NodeSeq.Empty).left.get mustBe an[EmptyMovementReferenceNumber]
    }

  }

  "correctRootNodeR" - {
    "returns true if the root node is as expected" in {

      val movement =
        <CC007A></CC007A>

      XmlMessageParser.correctRootNodeR(MessageType.ArrivalNotification)(movement).right.get mustBe movement
    }

    "returns InvalidRootNode if the root node is not as expected" in {

      val movement =
        <Foo></Foo>

      XmlMessageParser.correctRootNodeR(MessageType.ArrivalNotification)(movement).left.get mustBe an[InvalidRootNode]
    }
  }

  "dateTimeOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC007A>

      XmlMessageParser.dateTimeOfPrepR(movement).right.get mustEqual LocalDateTime.of(dateOfPrep, timeOfPrep)

    }

    "will return a None when the date in the DatOfPreMES9 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep) ++ "1"}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC007A>

      XmlMessageParser.dateTimeOfPrepR(movement).left.get mustBe an[LocalDateParseFailure]
    }

    "will return a LocalTimeParseFailure when the date in the TimOfPreMES10 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "1"}</TimOfPreMES10>
        </CC007A>

      XmlMessageParser.dateTimeOfPrepR(movement).left.get mustBe an[LocalTimeParseFailure]
    }

    "will return a LocalDateParseFailure when the DatOfPreMES9 node is missing" in {

      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC007A>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC007A>

      XmlMessageParser.dateTimeOfPrepR(movement).left.get mustBe an[LocalDateParseFailure]

    }

    "will return a LocalTimeParseFailure when the TimOfPreMES10 node is missing" in {

      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC007A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        </CC007A>

      XmlMessageParser.dateTimeOfPrepR(movement).left.get mustBe an[LocalTimeParseFailure]

    }

  }

}
