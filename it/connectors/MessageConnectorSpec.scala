package connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import generators.ModelGenerators
import models.{Source, WebChannel, XmlChannel}
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
    with ModelGenerators {

  import MessageConnectorSpec._

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  override protected def portConfigKey: String = "microservice.services.eis.port"

  private def connector: MessageConnector = app.injector.instanceOf[MessageConnector]

  private val genSource: Gen[Source] = Gen.oneOf(Seq(WebChannel, XmlChannel))
  private val genStatusCodes: Gen[Int] = Gen.choose(400, 500)

  "MessageConnector" - {

    "return OK when post is successful" in {
      forAll(genSource) {
        source =>

          server.stubFor(
            post(urlEqualTo(url))
              .withHeader("Content-Type", equalTo("application/xml"))
              .willReturn(
                ok.withHeader("Content-Type", "application/xml")
              )
          )

          val result = connector.post("<CC007A>test</CC007A>", "MessageCode", source)

          whenReady(result) {
            response =>
              response mustBe 200
          }
      }

    }

    "return BadRequest when post is unsuccessful" in {
      forAll(genSource, genStatusCodes) {
        (source, status) =>

          server.stubFor(
            post(urlEqualTo(url))
              .withHeader("Content-Type", equalTo("application/xml"))
              .willReturn(
                aResponse()
                  .withStatus(status)
              )
          )

          val result = connector.post("<CC007A>test</CC007A>", "MessageCode", source)

          whenReady(result) {
            response =>
              response mustBe 400
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
