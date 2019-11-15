package connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.mockito.MockitoSugar
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.xml.NodeSeq

class MessageConnectorSpec extends FreeSpec with MockitoSugar with ScalaFutures with MustMatchers with IntegrationPatience with WiremockSuite {

  import MessageConnectorSpec._

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  override protected def portConfigKey: String = "microservice.services.eis.port"

  def connector: MessageConnector = app.injector.instanceOf[MessageConnector]

  "MessageConnector" - {

    "return OK when post is successful" in {

      server.stubFor(
        post(urlEqualTo(url))
          .withHeader("Content-Type", equalTo("application/xml"))
          //.withRequestBody(equalTo("<CC007A>test</CC007A>"))
          .willReturn(
            ok.withHeader("Content-Type", "application/xml")
          )
      )

      val result = connector.post("<CC007A>test</CC007A>")

      whenReady(result) {
        response =>
          response mustBe 200
      }

    }

  }

}

object MessageConnectorSpec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private implicit val rh: RequestHeader = FakeRequest("", "")

  private val url = "/message-notification"
}
