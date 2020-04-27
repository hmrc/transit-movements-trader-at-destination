package connectors

import java.time.{LocalDateTime, OffsetDateTime}
import java.time.format.DateTimeFormatter

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import generators.MessageGenerators
import models.{ArrivalId, MessageSender, MessageState, MessageType, MovementMessageWithState, TransitWrapper}
import org.scalacheck.Gen
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class MessageConnectorSpec
    extends FreeSpec
    with MockitoSugar
    with ScalaFutures
    with MustMatchers
    with IntegrationPatience
    with WiremockSuite
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with OptionValues {

  import MessageConnectorSpec._

  override protected def portConfigKey: String = "microservice.services.eis.port"

  private def connector: MessageConnector = app.injector.instanceOf[MessageConnector]

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private val messageType: MessageType = Gen.oneOf(MessageType.values).sample.value

  "MessageConnector" - {

    "post" - {

      "return HttpResponse with status Accepted when when post is successful with Accepted" in {

        val messageSender = "MDTP-000000000000000000000000123-01"

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader("Authorization", equalTo("Bearer securityToken"))
            .withHeader("X-Forwarded-Host", equalTo("mdtp"))
            .withHeader("X-Correlation-ID", headerCarrierPattern)
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/xml"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withRequestBody(matchingXPath("/transitRequest"))
            .willReturn(
              aResponse()
                .withStatus(202)
            )
        )

        val postValue = MovementMessageWithState(LocalDateTime.now(), messageType, <CC007A>test</CC007A>, MessageState.SubmissionPending, 1)
        val arrivalId = ArrivalId(123)

        val result = connector.post(arrivalId, postValue, OffsetDateTime.now())

        whenReady(result) {
          response =>
            response.status mustBe 202
        }
      }

      "return an exception when post is unsuccessful" in {

        val messageSender = "MDTP-123-1"

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Correlation-ID", headerCarrierPattern)
            .withHeader("X-Forwarded-Host", equalTo("mdtp"))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withHeader("Accept", equalTo("application/xml"))
            .willReturn(
              aResponse()
                .withStatus(genFailedStatusCodes.sample.value)
            )
        )

        val postValue = MovementMessageWithState(LocalDateTime.now(), messageType, <CC007A>test</CC007A>, MessageState.SubmissionPending, 1)
        val arrivalId = ArrivalId(123)

        val result = connector.post(arrivalId, postValue, OffsetDateTime.now())

        whenReady(result.failed) {
          response =>
            response mustBe an[Exception]
        }
      }
    }
  }
}

object MessageConnectorSpec {

  private def headerCarrierPattern()(implicit headerCarrier: HeaderCarrier): StringValuePattern =
    headerCarrier.sessionId match {
      case Some(_) => equalTo("sessionId")
      case _       => matching("""\b[0-9a-f]{8}\b-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-\b[0-9a-f]{12}\b""")
    }

  private val postUrl = "/common-transit-convention-trader-at-destination/message-notification"

  private val headerCarrierWithSessionId = HeaderCarrier(sessionId = Some(SessionId("sessionId")))
  private val headerCarrier              = HeaderCarrier()

  private val genFailedStatusCodes: Gen[Int] = Gen.choose(400, 599)

}
