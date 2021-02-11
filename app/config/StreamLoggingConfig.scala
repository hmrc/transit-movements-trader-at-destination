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

package config

import akka.event.Logging
import akka.event.Logging.LogLevel
import play.api.Configuration

import javax.inject.Inject

trait StreamLoggingConfig {

  /**
    * Configuration for the logging level of AkkaStream components. This allows for global
    * application configuration, or stream component specific configuration.
    *
    * @note Valid values are: "error", "warning", "info", "debug"
    *
    * @param streamComponentName if provided, the value at
    *                            `data.stream.logging.${streamComponentName}.${on event}.level`
    *                            will be used for this component over the global value at
    *                            `data.stream.logging.onElement.level`,
    *                            `data.stream.logging.onFinish.level` or
    *                            `data.stream.logging.onFailure.level`
    *
    * @return (onElement, onFinish, onFailure)
    */
  def loggingConfig(streamComponentName: Option[String] = None): (LogLevel, LogLevel, LogLevel)

}

object StreamLoggingConfig {

  def getLogLevel(string: String): LogLevel =
    Logging
      .levelFor(string)
      .getOrElse(throw new InvalidConfigurationError("Invalid value of: " + string))

}

class StreamLoggingConfigImpl @Inject()(config: Configuration) extends StreamLoggingConfig {

  private val pathToConfig: String = "data.stream.logging"

  override def loggingConfig(streamComponentName: Option[String] = None): (LogLevel, LogLevel, LogLevel) =
    (
      onElement(streamComponentName),
      onFinish(streamComponentName),
      onFailure(streamComponentName)
    )

  private def streamLoggingConfigPath(streamComponentName: Option[String]): String =
    streamComponentName.fold(pathToConfig) {
      name =>
        s"$pathToConfig.$name"
    }

  private def onElement(streamComponentName: Option[String]): LogLevel =
    config
      .getOptional[LogLevel](streamLoggingConfigPath(streamComponentName) + ".onElement")
      .getOrElse(Logging.DebugLevel)

  private def onFinish(streamComponentName: Option[String]): LogLevel =
    config
      .getOptional[LogLevel](streamLoggingConfigPath(streamComponentName) + ".onFinish")
      .getOrElse(Logging.DebugLevel)

  private def onFailure(streamComponentName: Option[String]): LogLevel =
    config
      .getOptional[LogLevel](streamLoggingConfigPath(streamComponentName) + ".onFailure")
      .getOrElse(Logging.DebugLevel)

}
