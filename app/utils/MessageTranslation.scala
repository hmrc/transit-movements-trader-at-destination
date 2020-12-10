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

package utils

import config.AppConfig
import javax.inject.Inject
import play.api.Environment
import play.api.libs.json.Json
import play.api.libs.json.OFormat

import scala.io.Source

class MessageTranslation @Inject()(env: Environment, config: AppConfig) {

  private val nodes: List[NodeMap] = {
    val json =
      env
        .resourceAsStream(config.messageTranslationFile)
        .fold(throw new Exception(s"Could not read ${config.messageTranslationFile}"))(Source.fromInputStream)
        .mkString

    Json.parse(json).as[List[NodeMap]]
  }

  def translate(input: String): String =
    nodes.foldLeft(input) {
      (current, node) =>
        current.replace(node.field, node.description)
    }
}

case class NodeMap(field: String, description: String)

object NodeMap {
  implicit val format: OFormat[NodeMap] = Json.format[NodeMap]
}
