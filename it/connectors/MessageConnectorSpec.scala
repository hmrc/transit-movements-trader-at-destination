package connectors

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import generators.MessageGenerators
import models.request.ArrivalNotificationRequest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
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
    with MessageGenerators {

  import MessageConnectorSpec._

  override protected def portConfigKey: String = "microservice.services.eis.port"

  private def connector: MessageConnector = app.injector.instanceOf[MessageConnector]

  "MessageConnector" - {

    "return OK when post is successful" in {

      forAll(arbitrary[ArrivalNotificationRequest], genHeaderCarrier) {
        (arrivalNotificationRequest, hc) =>

          implicit val headerCarrier: HeaderCarrier = hc

          val xMessageType: String = arrivalNotificationRequest.xMessageType.code
          val messageSender = "mdtp-userseori"
          server.stubFor(
            post(urlEqualTo(url))

              .withHeader("Content-Type", equalTo("application/xml;charset=UTF-8"))
              .withHeader("X-Message-Type", equalTo(xMessageType))
              .withHeader("X-Correlation-ID", headerCarrierPattern)
              .withHeader("X-Forwarded-Host", equalTo("mdtp"))
              .withHeader("Date", equalTo(s"$dateTimeFormatted"))
              .withHeader("X-Message-Sender", equalTo(messageSender))
              .withHeader("Accept", equalTo("application/xml"))
              .withHeader("Authorization", equalTo("Bearer bearertokenhere"))
              .willReturn(
                aResponse()
                  .withStatus(200)
              )
          )

          val result = connector.post("<CC007A>test</CC007A>", arrivalNotificationRequest.xMessageType, localDateTime)

          whenReady(result) {
            response =>
              response.status mustBe 200
          }
      }
    }


    "return an exception when post is unsuccessful" in {
      forAll(genFailedStatusCodes, arbitrary[ArrivalNotificationRequest], genHeaderCarrier) {
        (statusCode, arrivalNotificationRequest, hc) =>

          implicit val headerCarrier: HeaderCarrier = hc

          val xMessageType: String = arrivalNotificationRequest.xMessageType.code
          val messageSender = "mdtp-userseori"

          server.stubFor(
            post(urlEqualTo(url))
              .withHeader("Content-Type", equalTo("application/xml;charset=UTF-8"))
              .withHeader("X-Message-Type", equalTo(xMessageType))
              .withHeader("X-Correlation-ID", headerCarrierPattern)
              .withHeader("X-Forwarded-Host", equalTo("mdtp"))
              .withHeader("X-Message-Sender", equalTo(messageSender))
              .withHeader("Date", equalTo("test"))
              .withHeader("Accept", equalTo("application/xml"))
              .withHeader("Authorization", equalTo("Bearer bearertokenhere"))
              .willReturn(
                aResponse()
                  .withStatus(statusCode)
              )
          )

          val result = connector.post("<CC007A>test</CC007A>", arrivalNotificationRequest.xMessageType, localDateTime)

          whenReady(result.failed) {
            response =>
              response mustBe an[Exception]
          }
      }
    }
  }
}

object MessageConnectorSpec {

  private def headerCarrierPattern()(implicit headerCarrier: HeaderCarrier): StringValuePattern = {
    headerCarrier.sessionId match {
      case Some(_) => equalTo("sessionId")
      case _ => matching("""\b[0-9a-f]{8}\b-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-\b[0-9a-f]{12}\b""")
    }
  }

  private val url = "/common-transit-convention-trader-at-destination/message-notification"

  private val headerCarrierWithSessionId = HeaderCarrier(sessionId = Some(SessionId("sessionId")))
  private val headerCarrier = HeaderCarrier()

  private val genFailedStatusCodes: Gen[Int] = Gen.choose(400, 599)
  private val genHeaderCarrier = Gen.oneOf(Seq(headerCarrierWithSessionId, headerCarrier))

  private val localDateTime = OffsetDateTime.now
  private val dateFormatter: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME
  private val dateTimeFormatted: String = localDateTime.format(dateFormatter)
}
