/*
 * Copyright 2020 HM Revenue & Customs
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

import base.SpecBase
import com.github.tomakehurst.wiremock.client.WireMock.aResponse
import com.github.tomakehurst.wiremock.client.WireMock.post
import com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo
import generators.ModelGenerators
import models.response.ResponseMovementMessage
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.Future

class ManageDocumentsConnectorSpec extends SpecBase with WiremockSuite with ScalaCheckPropertyChecks with ModelGenerators {

  override protected def portConfigKey: String = "microservice.services.manage-documents.port"

  lazy val connector: ManageDocumentsConnector = app.injector.instanceOf[ManageDocumentsConnector]

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "ManageDocumentsConnector" - {

    "getUnloadingPermissionPdf" - {

      "must return status Ok and PDF" in {

        server.stubFor(
          post(urlEqualTo("/transit-movements-trader-manage-documents/unloading-permission"))
            .willReturn(
              aResponse()
                .withStatus(200)
            )
        )

        forAll(arbitrary[ResponseMovementMessage]) {
          responseMovementMessage =>
            val result: Future[HttpResponse] = connector.getUnloadingPermissionPdf(responseMovementMessage)
            result.futureValue.status mustBe OK
        }
      }
    }
  }

}
