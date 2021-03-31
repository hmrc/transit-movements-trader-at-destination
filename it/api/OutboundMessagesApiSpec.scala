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

package api

import cats.data.NonEmptyList

import play.api.test.Helpers.ACCEPTED
import play.api.test.Helpers.BAD_REQUEST
import generators.ModelGenerators
import models.ArrivalStatus.UnloadingPermission
import models.{Arrival, MessageStatus, MessageType, MovementMessageWithStatus, MovementMessageWithoutStatus}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.test.{DefaultAwaitTimeout, FutureAwaits}
import repositories.ArrivalMovementRepository
import utils.Format

import java.time.LocalDateTime

class OutboundMessagesApiSpec extends AnyFreeSpec with GuiceOneServerPerSuite with Matchers
  with ScalaCheckPropertyChecks
  with ModelGenerators
  with OptionValues
  with EitherValues
  with FutureAwaits
  with DefaultAwaitTimeout
  with ScalaFutures
  with WiremockSuite
{

  override protected def portConfigKeys: Seq[String] = Seq(
    "microservice.services.auth.port",
    "microservice.services.eis.port"
  )

  override implicit lazy val app: Application = appBuilder.build()

  lazy val ws: WSClient = app.injector.instanceOf[WSClient]
  lazy val repo: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

  "postMessage" - {
    "return a Bad request if a second request comes in for the same unloading remarks" in {

      val movements = NonEmptyList(
        MovementMessageWithStatus(LocalDateTime.now(), MessageType.ArrivalNotification, <CC007A></CC007A>, MessageStatus.SubmissionSucceeded, 1),
        MovementMessageWithoutStatus(LocalDateTime.now(), MessageType.UnloadingPermission, <CC043A></CC043A>, 1) :: Nil
      )

      val arrival = arbitrary[Arrival].sample.value.copy(messages = movements, status = UnloadingPermission)

      val requestBody = <CC044A>
        <DatOfPreMES9>{Format.dateFormatted(LocalDateTime.now())}</DatOfPreMES9>
        <TimOfPreMES10>{Format.timeFormatted(LocalDateTime.now())}</TimOfPreMES10>
        <SynVerNumMES2>1</SynVerNumMES2>
        <HEAHEA>
          <DocNumHEA5>{arrival.movementReferenceNumber.value}</DocNumHEA5>
        </HEAHEA>
      </CC044A>

      repo.insert(arrival).futureValue

      Stubs(server)
        .successfulAuth(arrival.eoriNumber)
        .successfulSubmission()
        .build()

      await(ws
        .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages")
        .withHttpHeaders(
          "Channel" -> arrival.channel.toString,
          "Content-Type" -> "application/xml"
        )
        .post(requestBody)).status mustBe ACCEPTED

      await(ws
        .url(s"http://localhost:$port/transit-movements-trader-at-destination/movements/arrivals/${arrival.arrivalId.index}/messages")
        .withHttpHeaders(
          "Channel" -> arrival.channel.toString,
          "Content-Type" -> "application/xml"
        )
        .post(requestBody)).status mustBe BAD_REQUEST
    }
  }


}
