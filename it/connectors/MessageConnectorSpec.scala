package connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import generators.MessageGenerators
import models.messages.request.ArrivalNotificationRequest
import models.{Source, WebChannel, XmlChannel}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier

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

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  override protected def portConfigKey: String = "microservice.services.eis.port"

  private def connector: MessageConnector = app.injector.instanceOf[MessageConnector]

  private val genSource: Gen[Source] = Gen.oneOf(Seq(WebChannel, XmlChannel))
  private val genFailedStatusCodes: Gen[Int] = Gen.choose(400, 599)
  private val genSuccessfulStatusCodes: Gen[Int] = Gen.choose(200, 299)

  "MessageConnector" - {

    "return OK when post is successful" in {
      forAll(genSource, genSuccessfulStatusCodes, arbitrary[ArrivalNotificationRequest]) {
        (source, status, arrivalNotificationRequest) =>

          server.stubFor(
            post(urlEqualTo(url))
              .withHeader("Content-Type", equalTo("application/xml"))
              .willReturn(
                aResponse()
                  .withStatus(status)
                  .withHeader("Content-Type", "application/xml")
              )
          )

          val result = connector.post("<CC007A>test</CC007A>", arrivalNotificationRequest.messageCode, source)

          whenReady(result) {
            response =>
              response.status mustBe status
          }
      }

    }

    "return BadRequest when post is unsuccessful" in {
      forAll(genSource, genFailedStatusCodes, arbitrary[ArrivalNotificationRequest]) {
        (source, status, arrivalNotificationRequest) =>

          server.stubFor(
            post(urlEqualTo(url))
              .withHeader("Content-Type", equalTo("application/xml"))
              .willReturn(
                aResponse()
                  .withStatus(status)
              )
          )

          val result = connector.post("<CC007A>test</CC007A>", arrivalNotificationRequest.messageCode, source)

          whenReady(result.failed) {
            response =>
              response mustBe an[Exception]
          }
      }
    }
  }

}

object MessageConnectorSpec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private implicit val rh: RequestHeader = FakeRequest("", "")

  private val url = "/message-notification"
}
