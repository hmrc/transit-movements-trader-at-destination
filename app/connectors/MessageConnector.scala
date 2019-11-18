/*
 * Copyright 2019 HM Revenue & Customs
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

import com.google.inject.Inject
import config.AppConfig
import models.Source
import play.api.mvc.RequestHeader
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}


class MessageConnectorImpl @Inject()(config: AppConfig, http: HttpClient) extends MessageConnector {

  // TODO consider creating HttpReads to retrieve response status (to prevent the future from failing)

  def post(xml: String, messageCode: String, source: Source)(implicit  headerCarrier: HeaderCarrier,
             ec: ExecutionContext,
             request: RequestHeader): Future[HttpResponse] = {

    val url = config.eisUrl
    val customHeaders: Seq[(String, String)] = Seq(
      "Content-Type" -> "application/xml",
      "Accept" -> "application/xml",
      "Source" -> source.channel,
      "MessageCode" -> messageCode
    )

    http.POST(url, xml, customHeaders)
  }
}

trait MessageConnector {
  def post(xml: String, messageCode: String, source: Source)(implicit
             headerCarrier: HeaderCarrier,
             ec: ExecutionContext,
             request: RequestHeader): Future[HttpResponse]
}
