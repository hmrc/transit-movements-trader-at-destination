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

package utils

import com.google.inject.Inject
import config.AppConfig
import logging.Logging
import org.json.XML
import play.api.libs.json.JsObject
import play.api.libs.json.Json

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml.NodeSeq

trait XmlToJsonConverter {
  def toJson(xml: NodeSeq): JsObject
}

class ConfiguredXmlToJsonConverter @Inject()(appConfig: AppConfig) extends Logging with XmlToJsonConverter {

  def toJson(xml: NodeSeq): JsObject =
    if (appConfig.xmlToJson) {
      Try(Json.parse(XML.toJSONObject(xml.toString).toString).as[JsObject]) match {
        case Success(data) => data
        case Failure(error) =>
          logger.error(s"Failed to convert xml to json with error: ${error.getMessage}")
          Json.obj()
      }
    } else Json.obj()
}
