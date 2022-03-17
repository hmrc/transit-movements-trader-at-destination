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

package audit

import base.SpecBase
import cats.data.Ior
import generators.ModelGenerators
import models.ChannelType
import models.EORINumber
import models.MessageId
import models.MessageStatus
import models.MessageType
import models.MovementMessageWithStatus
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import config.Constants
import play.api.inject.bind
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import utils.XMLTransformer.toJson
import utils.Format
import utils.MessageTranslation

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

class ArrivalNotificationAuditDetailsSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach with ModelGenerators {

  val mockMessageTranslation: MessageTranslation = mock[MessageTranslation]

  "ArrivalNotificationAuditDetails" - {

    val requestXml =
      <CC007A>
        <SynVerNumMES2>123</SynVerNumMES2>
        <DatOfPreMES9>
          {Format.dateFormatted(LocalDate.now())}
        </DatOfPreMES9>
        <TimOfPreMES10>
          {Format.timeFormatted(LocalTime.of(1, 1))}
        </TimOfPreMES10>
        <HEAHEA>
          <ArrAutLocOfGooHEA65>location</ArrAutLocOfGooHEA65>
          <CONNR3>container</CONNR3>
          <CONNR3>container</CONNR3>
          <CONNR3>container</CONNR3>
          <CONNR3>container</CONNR3>
          <CONNR3>container</CONNR3>
        </HEAHEA>
      </CC007A>

    val enrolmentId = Ior.right(EORINumber(Constants.NewEnrolmentIdKey))
    val movementMessage =
      MovementMessageWithStatus(MessageId(1), LocalDateTime.now, MessageType.ArrivalNotification, requestXml, MessageStatus.SubmissionSucceeded, 1)

    val statistics = (requestLength: Int) =>
      Json.obj(
        "authorisedLocationOfGoods" -> "location",
        "totalNoOfContainers"       -> 5,
        "requestLength"             -> requestLength
    )

    "must include translated xml when request size is less than max size allowed and generate xml statistics" in {

      val requestSize = ArrivalNotificationAuditDetails.maxRequestLength - 1000

      val application = baseApplicationBuilder
        .overrides(bind[MessageTranslation].toInstance(mockMessageTranslation))
        .build()
      val messageTranslation = application.injector.instanceOf[MessageTranslation]
      val jsonMessage        = messageTranslation.translate(toJson(movementMessage.message))

      val expectedDetails = Json.obj(
        "channel"       -> "api",
        "customerId"    -> "EORINumber",
        "enrolmentType" -> "HMRC-CTC-ORG",
        "message"       -> jsonMessage,
        "statistics"    -> statistics(requestSize)
      )

      val details = ArrivalNotificationAuditDetails(ChannelType.api, enrolmentId, movementMessage.message, requestSize, mockMessageTranslation)

      Json.toJson(details).as[JsObject] mustEqual expectedDetails
    }

    "must include message to indicate request size is more than max size allowed and generate xml statistics" in {

      val requestSize = ArrivalNotificationAuditDetails.maxRequestLength + 1000

      val expectedDetails = Json.obj(
        "channel"       -> "api",
        "customerId"    -> "EORINumber",
        "enrolmentType" -> "HMRC-CTC-ORG",
        "message"       -> Json.obj("arrivalNotification" -> "Arrival notification too large to be included"),
        "statistics"    -> statistics(requestSize)
      )

      val details = ArrivalNotificationAuditDetails(ChannelType.api, enrolmentId, movementMessage.message, requestSize, mockMessageTranslation)

      Json.toJson(details).as[JsObject] mustEqual expectedDetails
    }
  }
}
