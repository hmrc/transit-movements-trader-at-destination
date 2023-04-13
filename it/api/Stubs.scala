/*
 * Copyright 2023 HM Revenue & Customs
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
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import models.ChannelType
import models.MessageType
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import play.api.libs.json.Json

case class Stubs(server: WireMockServer, private val stubs: Seq[() => StubMapping] = Nil) {

  def successfulAuth(eori: String): Stubs = copy(stubs =
    stubs :+ (
      (
      ) =>
        server.stubFor(
          post(urlEqualTo("/auth/authorise"))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withHeader("Content-Type", "application/json")
                .withBody(
                  Json
                    .obj(
                      "authorisedEnrolments" -> Json.arr(
                        Json.obj(
                          "key" -> "HMCE-NCTS-ORG",
                          "identifiers" -> Json.arr(
                            Json.obj("key" -> "VATRegNoTURN", "value" -> eori)
                          ),
                          "state" -> "Activated"
                        )
                      )
                    )
                    .toString()
                    .getBytes
                )
            )
        )
    )
  )

  def successfulSubmission(): Stubs = copy(stubs =
    stubs :+ (
      (
      ) =>
        server.stubFor(
          post(urlEqualTo("/movements/messages"))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader(HeaderNames.USER_AGENT, equalTo("transit-movements-trader-at-destination"))
            .withHeader("X-Message-Sender", matching("MDTP-ARR-\\d{23}-\\d{2}"))
            .withHeader("X-Message-Type", matching(MessageType.values.map(_.code).mkString("(", "|", ")")))
            .withHeader("channel", matching(ChannelType.values.map(_.toString).mkString("(", "|", ")")))
            .willReturn(
              aResponse()
                .withStatus(202)
            )
        )
    )
  )

  def build(): Seq[StubMapping] = stubs.map(_())
}
