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

package testOnly.services

import cats.data.NonEmptyList
import models.ArrivalStatus.Initialized
import models.MessageType.ArrivalNotification
import models.Arrival
import models.ArrivalId
import models.ChannelType
import models.MessageStatus
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import testOnly.models.SeedDataParameters

import java.time.Clock
import java.time.LocalDateTime
import scala.xml.Elem

private[testOnly] object TestOnlySeedDataService {

  def seedArrivals(seedDataParameters: SeedDataParameters, clock: Clock): Iterator[Arrival] =
    seedDataParameters.seedData
      .map {
        case (id, eori, mrn) =>
          makeArrivalMovement(eori.format, mrn.format, id, clock)
      }

  private def makeArrivalMovement(eori: String, mrn: String, arrivalId: ArrivalId, clock: Clock): Arrival = {

    import scala.xml.XML

    val dateTime = LocalDateTime.now(clock)

    val xml         = XML.loadFile("app/testOnly/xml/arrivals.xml")
    val modifiedXml = replaceElements(xml, eori, mrn)

    val movementMessage = MovementMessageWithStatus(dateTime, ArrivalNotification, modifiedXml, MessageStatus.SubmissionPending, 1)

    Arrival(arrivalId, ChannelType.web, MovementReferenceNumber(mrn), eori, Initialized, dateTime, dateTime, dateTime, NonEmptyList.one(movementMessage), 2)
  }

  private def replaceElements(xml: Elem, eori: String, mrn: String) =
    scala.xml.XML.loadString(
      xml
        .toString()
        .replace("<DocNumHEA5/>", s"<DocNumHEA5>$mrn</DocNumHEA5>")
        .replace("<TINTRD59/>", s"<TINTRD59>$eori</TINTRD59>")
    )
}
