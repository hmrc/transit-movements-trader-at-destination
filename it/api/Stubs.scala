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

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, post, urlEqualTo}
import play.api.libs.json.Json

case class Stubs(server: WireMockServer, private val stubs: Seq[() => StubMapping] = Nil){
    def successfulAuth(eori: String): Stubs = copy(stubs = stubs :+ (() => server.stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withHeader("Content-Type", "application/json")
            .withBody(Json.obj("authorisedEnrolments" -> Json.arr(
              Json.obj("key" -> "HMCE-NCTS-ORG",
                "identifiers" -> Json.arr(
                  Json.obj("key" -> "VATRegNoTURN",
                    "value" -> eori)
                ),
                "state" -> "Activated"
              )
            )).toString().getBytes)))
        )
    )

    def successfulSubmission(): Stubs = copy(stubs = stubs :+ (() => server.stubFor(
      post(urlEqualTo("/movements/messages"))
        .willReturn(
          aResponse()
            .withStatus(202)
        ))
      )
    )


    def build(): Seq[StubMapping] = stubs.map(_())
  }