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

package api

import cats.data.NonEmptyList
import cats.syntax.all._
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.MessageId
import models.MessageStatus
import models.MessageType
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.libs.json.JsNumber
import play.api.libs.ws.WSClient
import play.api.test.DefaultAwaitTimeout
import play.api.test.FutureAwaits
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import utils.Format

import java.net.URLEncoder
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext.Implicits.global

class OutboundMessagesApiSpec
    extends AnyFreeSpec
    with GuiceOneServerPerSuite
    with Matchers
    with ModelGenerators
    with OptionValues
    with FutureAwaits
    with DefaultAwaitTimeout
    with WiremockSuite {

  override protected def portConfigKeys: Seq[String] = Seq(
    "microservice.services.auth.port",
    "microservice.services.eis.port"
  )

  lazy val ws: WSClient                    = app.injector.instanceOf[WSClient]
  lazy val repo: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

  "postMessage" - {
    "return ACCEPTED if a second request comes in for the same unloading remarks" in {

      val movements = NonEmptyList(
        MovementMessageWithStatus(MessageId(1), LocalDateTime.now(), MessageType.ArrivalNotification, <CC007A></CC007A>, MessageStatus.SubmissionSucceeded, 1),
        MovementMessageWithoutStatus(MessageId(2), LocalDateTime.now(), MessageType.UnloadingPermission, <CC043A></CC043A>, 1) :: Nil
      )

      val arrival =
        arbitrary[Arrival].sample.value.copy(messages = movements, lastUpdated = LocalDateTime.of(1998, 4, 30, 9, 30, 31))

      val requestBody = <CC044A>
        <DatOfPreMES9>{Format.dateFormatted(LocalDateTime.now())}</DatOfPreMES9>
        <TimOfPreMES10>{Format.timeFormatted(LocalDateTime.now())}</TimOfPreMES10>
        <SynVerNumMES2>1</SynVerNumMES2>
        <HEAHEA>
          <DocNumHEA5>{arrival.movementReferenceNumber.value}</DocNumHEA5>
        </HEAHEA>
      </CC044A>

      await(repo.insert(arrival))

      Stubs(server)
        .successfulAuth(arrival.eoriNumber)
        .successfulSubmission()
        .build()

      await(
        ws
          .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages")
          .withHttpHeaders(
            "Channel"       -> arrival.channel.toString,
            "Content-Type"  -> "application/xml",
            "Authorization" -> "Bearer token"
          )
          .post(requestBody)
      ).status mustBe ACCEPTED

      await(
        ws
          .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages")
          .withHttpHeaders(
            "Channel"       -> arrival.channel.toString,
            "Content-Type"  -> "application/xml",
            "Authorization" -> "Bearer token"
          )
          .post(requestBody)
      ).status mustBe ACCEPTED
    }
  }

  "getArrivals" - {
    "accepts updatedSince parameter in valid date format" in {
      import models.ChannelType._

      val eoriNumber: String = arbitrary[String].sample.value

      val arrivalMovement1 =
        arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(0),
          eoriNumber = eoriNumber,
          channel = api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31)
        )
      val arrivalMovement2 =
        arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(1),
          eoriNumber = eoriNumber,
          channel = api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 35, 32)
        )
      val arrivalMovement3 =
        arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(2),
          eoriNumber = eoriNumber,
          channel = api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 21)
        )
      val arrivalMovement4 =
        arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(3),
          eoriNumber = eoriNumber,
          channel = api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 10, 15, 16)
        )

      await(List(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4).traverse(repo.insert))

      Stubs(server)
        .successfulAuth(eoriNumber)
        .build

      val dateTime    = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
      val dateTimeStr = DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(dateTime)
      val encodedStr  = URLEncoder.encode(dateTimeStr, "UTF-8")

      val response = await(
        ws
          .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals?updatedSince=$encodedStr")
          .withHttpHeaders(
            "Channel"       -> api.toString,
            "Content-Type"  -> "application/xml",
            "Authorization" -> "Bearer token"
          )
          .get()
      )

      response.status mustBe OK

      (response.json \\ "arrivalId").toSet mustBe Set(JsNumber(arrivalMovement2.arrivalId.index), JsNumber(arrivalMovement4.arrivalId.index))
    }

    "does not require updatedSince parameter" in {
      import models.ChannelType._

      val eoriNumber: String = arbitrary[String].sample.value

      Stubs(server)
        .successfulAuth(eoriNumber)
        .build

      val response = await(
        ws
          .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals")
          .withHttpHeaders(
            "Channel"       -> api.toString,
            "Content-Type"  -> "application/xml",
            "Authorization" -> "Bearer token"
          )
          .get()
      )

      response.status mustBe OK
    }

    "rejects updatedSince parameter in invalid date format" in {
      import models.ChannelType._

      val eoriNumber: String = arbitrary[String].sample.value

      Stubs(server)
        .successfulAuth(eoriNumber)
        .build

      val dateTime    = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
      val dateTimeStr = DateTimeFormatter.RFC_1123_DATE_TIME.format(dateTime)
      val encodedStr  = URLEncoder.encode(dateTimeStr, "UTF-8")

      val response = await(
        ws
          .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals?updatedSince=$encodedStr")
          .withHttpHeaders(
            "Channel"       -> api.toString,
            "Content-Type"  -> "application/xml",
            "Authorization" -> "Bearer token"
          )
          .get()
      )

      response.status mustBe BAD_REQUEST
    }
  }

}
