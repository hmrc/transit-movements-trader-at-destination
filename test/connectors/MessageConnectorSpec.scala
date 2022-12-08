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

package connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import connectors.MessageConnector.EisSubmissionResult.DownstreamBadGateway
import connectors.MessageConnector.EisSubmissionResult.DownstreamInternalServerError
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionSuccessful
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import connectors.MessageConnector.EisSubmissionResult.VirusFoundOrInvalidToken
import generators.ModelGenerators
import models.ArrivalId
import models.ChannelType.web
import models.MessageId
import models.MessageStatus
import models.MessageType
import models.MovementMessageWithStatus
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import play.api.test.Helpers.running
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import java.time.OffsetDateTime
import uk.gov.hmrc.http.RequestId

class MessageConnectorSpec
    extends AnyFreeSpec
    with MockitoSugar
    with ScalaFutures
    with Matchers
    with IntegrationPatience
    with WiremockSuite
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  import MessageConnectorSpec._

  override protected def portConfigKey: String = "microservice.services.eis.port"

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier().copy(requestId = Some(RequestId("bar")), otherHeaders = Seq("X-Client-Id" -> "foo"))

  private val messageType: MessageType = Gen.oneOf(MessageType.values).sample.value

  private val messageSender = "MDTP-ARR-00000000000000000000123-01"

  "MessageConnector" - {

    "post" - {

      "return SubmissionSuccess when when post is successful with Accepted" in {

        val messageSender = "MDTP-ARR-00000000000000000000123-01"

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("channel", equalTo(web.toString))
            .withHeader(HeaderNames.ACCEPT, equalTo("application/xml"))
            .withRequestBody(matchingXPath("/transitRequest"))
            .willReturn(
              aResponse()
                .withStatus(202)
            )
        )

        val postValue = MovementMessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          Some(LocalDateTime.now()),
          messageType,
          <CC007A>test</CC007A>,
          MessageStatus.SubmissionPending,
          1
        )
        val arrivalId = ArrivalId(123)

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[MessageConnector]

          val result = connector.post(arrivalId, postValue, OffsetDateTime.now(), web)

          result.futureValue mustEqual EisSubmissionSuccessful
        }
      }

      "return a ErrorInPayload for a return code of 400" in {

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("channel", equalTo(web.toString))
            .withHeader(HeaderNames.ACCEPT, equalTo("application/xml"))
            .willReturn(
              aResponse()
                .withStatus(400)
            )
        )

        val postValue = MovementMessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          Some(LocalDateTime.now()),
          messageType,
          <CC007A>test</CC007A>,
          MessageStatus.SubmissionPending,
          1
        )
        val arrivalId = ArrivalId(123)

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[MessageConnector]

          val result = connector.post(arrivalId, postValue, OffsetDateTime.now(), web)

          result.futureValue mustEqual ErrorInPayload
        }
      }

      "return a VirusFoundOrInvalidToken for a return code of 403" in {

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("channel", equalTo(web.toString))
            .withHeader(HeaderNames.ACCEPT, equalTo("application/xml"))
            .willReturn(
              aResponse()
                .withStatus(403)
            )
        )

        val postValue = MovementMessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          Some(LocalDateTime.now()),
          messageType,
          <CC007A>test</CC007A>,
          MessageStatus.SubmissionPending,
          1
        )
        val arrivalId = ArrivalId(123)

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[MessageConnector]

          val result = connector.post(arrivalId, postValue, OffsetDateTime.now(), web)

          result.futureValue mustEqual VirusFoundOrInvalidToken
        }
      }

      "return a DownstreamFailure for a return code of 500" in {

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("channel", equalTo(web.toString))
            .withHeader(HeaderNames.ACCEPT, equalTo("application/xml"))
            .willReturn(
              aResponse()
                .withStatus(500)
            )
        )

        val postValue = MovementMessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          Some(LocalDateTime.now()),
          messageType,
          <CC007A>test</CC007A>,
          MessageStatus.SubmissionPending,
          1
        )
        val arrivalId = ArrivalId(123)

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[MessageConnector]

          val result = connector.post(arrivalId, postValue, OffsetDateTime.now(), web)

          result.futureValue mustEqual DownstreamInternalServerError
        }
      }

      "return a DownstreamBadGateway for a return code of 502" in {

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("channel", equalTo(web.toString))
            .withHeader(HeaderNames.ACCEPT, equalTo("application/xml"))
            .willReturn(
              aResponse()
                .withStatus(502)
            )
        )

        val postValue = MovementMessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          Some(LocalDateTime.now()),
          messageType,
          <CC007A>test</CC007A>,
          MessageStatus.SubmissionPending,
          1
        )
        val arrivalId = ArrivalId(123)

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[MessageConnector]

          val result = connector.post(arrivalId, postValue, OffsetDateTime.now(), web)

          result.futureValue mustEqual DownstreamBadGateway
        }
      }

      "return an UnexpectedHttpResonse for an error code other than 202, 400, 403, 500" in {

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("channel", equalTo(web.toString))
            .withHeader(HeaderNames.ACCEPT, equalTo("application/xml"))
            .willReturn(
              aResponse()
                .withStatus(418)
            )
        )

        val postValue = MovementMessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          Some(LocalDateTime.now()),
          messageType,
          <CC007A>test</CC007A>,
          MessageStatus.SubmissionPending,
          1
        )
        val arrivalId = ArrivalId(123)

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[MessageConnector]

          val result = connector.post(arrivalId, postValue, OffsetDateTime.now(), web)

          result.futureValue.statusCode mustEqual 418
        }
      }
    }
  }
}

object MessageConnectorSpec {

  private val postUrl = "/movements/messages"
}
