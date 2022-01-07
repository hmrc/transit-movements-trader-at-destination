/*
 * Copyright 2022 HM Revenue & Customs
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

import akka.event.Logging.LogLevel
import play.api.ConfigLoader
import play.api.Configuration

package object config {

  implicit lazy val configLoader: ConfigLoader[LogLevel] = ConfigLoader {
    config => prefix =>
      val configAtPath = Configuration(config).get[Configuration](prefix)
      val level        = configAtPath.get[String]("level")

      StreamLoggingConfig.getLogLevel(level)
  }

}
