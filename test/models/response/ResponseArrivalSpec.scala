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

package models

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Month

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.ChannelType.api
import models.MessageStatus.SubmissionPending
import models.response.ResponseArrival
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.Format

import scala.xml.NodeSeq
import scala.xml.Utility.trim

class ResponseArrivalSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {
  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val arrivalId = arbitrary[ArrivalId].sample.value
  val mrn       = arbitrary[MovementReferenceNumber].sample.value

  val requestXmlBody: NodeSeq =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <SynVerNumMES2>1</SynVerNumMES2>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>

  def savedXmlMessage(messageCorrelationId: Int) =
    <CC007A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <SynVerNumMES2>1</SynVerNumMES2>
      <MesSenMES3>{MessageSender(arrivalId, messageCorrelationId).toString}</MesSenMES3>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC007A>

  def movementMessage(messageCorrelationId: Int): MovementMessageWithStatus =
    MovementMessageWithStatus(
      MessageId(1),
      localDateTime,
      MessageType.ArrivalNotification,
      savedXmlMessage(messageCorrelationId).map(trim),
      SubmissionPending,
      1
    )(emptyConverter)

  val initializedArrival = Arrival(
    arrivalId = arrivalId,
    channel = api,
    movementReferenceNumber = mrn,
    eoriNumber = "eori",
    status = ArrivalStatus.Initialized,
    created = localDateTime,
    updated = LocalDateTime.of(2005, Month.FEBRUARY, 4, 0, 0),
    lastUpdated = localDateTime,
    nextMessageCorrelationId = movementMessage(1).messageCorrelationId + 1,
    messages = NonEmptyList.one(movementMessage(1)),
    notificationBox = None
  )

  "ResponseArrival" - {
    "must populate updated field from lastUpdated in Arrival" in {
      val responseArrival = ResponseArrival.build(initializedArrival)

      responseArrival.updated mustEqual initializedArrival.lastUpdated
      responseArrival.updated must not equal initializedArrival.updated
    }
  }
}
