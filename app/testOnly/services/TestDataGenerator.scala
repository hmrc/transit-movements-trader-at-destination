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
import models.Arrival
import models.ArrivalId
import models.ChannelType
import models.MessageStatus
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import models.ArrivalStatus.ArrivalSubmitted
import models.MessageType.ArrivalNotification
import testOnly.models.SeedEori
import testOnly.models.SeedMrn

import java.time.Clock
import java.time.LocalDateTime
import javax.inject.Inject
import scala.xml.Elem
import scala.xml.XML

private[services] class TestDataGenerator @Inject()(clock: Clock) {

  def arrivalMovement(eori: SeedEori, mrn: SeedMrn, arrivalId: ArrivalId): Arrival = {

    val dateTime = LocalDateTime.now(clock)

    val xml = TestDataXMLGenerator.arrivalNotification(mrn.format, eori.format)

    val movementMessage = MovementMessageWithStatus(dateTime, ArrivalNotification, xml, MessageStatus.SubmissionPending, 1)

    Arrival(
      arrivalId,
      ChannelType.web,
      MovementReferenceNumber(mrn.format),
      eori.format,
      ArrivalSubmitted,
      dateTime,
      dateTime,
      dateTime,
      NonEmptyList.one(movementMessage),
      2
    )
  }

}

object TestDataXMLGenerator {

  def arrivalNotification(mrn: String, eori: String): Elem =
    XML.loadString(
      s"""
      |<CC007A>
      |  <SynIdeMES1>UNOC</SynIdeMES1>
      |  <SynVerNumMES2>3</SynVerNumMES2>
      |  <MesRecMES6>NCTS</MesRecMES6>
      |  <DatOfPreMES9>20200519</DatOfPreMES9>
      |  <TimOfPreMES10>1357</TimOfPreMES10>
      |  <IntConRefMES11>WE190912102534</IntConRefMES11>
      |  <AppRefMES14>NCTS</AppRefMES14>
      |  <TesIndMES18>0</TesIndMES18>
      |  <MesIdeMES19>1</MesIdeMES19>
      |  <MesTypMES20>GB007A</MesTypMES20>
      |  <HEAHEA>
      |    <DocNumHEA5>$mrn</DocNumHEA5>
      |    <ArrNotPlaHEA60>DOVER</ArrNotPlaHEA60>
      |    <ArrNotPlaHEA60LNG>EN</ArrNotPlaHEA60LNG>
      |    <ArrAgrLocOfGooHEA63LNG>EN</ArrAgrLocOfGooHEA63LNG>
      |    <SimProFlaHEA132>1</SimProFlaHEA132>
      |    <ArrNotDatHEA141>20190912</ArrNotDatHEA141>
      |    <DiaLanIndAtDesHEA255>EN</DiaLanIndAtDesHEA255>
      |  </HEAHEA>
      |  <TRADESTRD>
      |    <CouTRD25>GB</CouTRD25>
      |    <NADLNGRD>EN</NADLNGRD>
      |    <TINTRD59>$eori</TINTRD59>
      |  </TRADESTRD>
      |  <CUSOFFPREOFFRES>
      |    <RefNumRES1>GB000060</RefNumRES1>
      |  </CUSOFFPREOFFRES>
      |</CC007A>
      |""".stripMargin
    )
}
