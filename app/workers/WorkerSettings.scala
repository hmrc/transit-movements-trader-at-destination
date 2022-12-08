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

package workers

import play.api.ConfigLoader
import play.api.Configuration

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

case class WorkerSettings(
  enabled: Boolean,
  interval: FiniteDuration,
  groupSize: Int,
  parallelism: Int,
  elements: Int,
  per: FiniteDuration
)

object WorkerSettings {

  implicit val reader: ConfigLoader[WorkerSettings] = ConfigLoader {
    config => prefix =>
      val configForWorker = Configuration(config).get[Configuration](prefix)

      WorkerSettings(
        enabled = configForWorker.get[Boolean]("enabled"),
        interval = FiniteDuration(configForWorker.get[Duration]("interval").toNanos, TimeUnit.NANOSECONDS),
        groupSize = configForWorker.get[Int]("group-size"),
        parallelism = configForWorker.get[Int]("parallelism"),
        elements = configForWorker.get[Int]("throttle.elements"),
        per = FiniteDuration(configForWorker.get[Duration]("throttle.per").toNanos, TimeUnit.NANOSECONDS)
      )
  }

}
