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

package workers

import javax.inject.Inject
import play.api.Configuration

import scala.concurrent.duration.FiniteDuration

class WorkerConfig @Inject()(config: Configuration) {

  val addJsonToMessagesWorkerSettings: WorkerSettings =
    WorkerSettings(
      enabled = config.get[Boolean]("workers.add-json-to-messages.enabled"),
      interval = config.get[FiniteDuration]("workers.add-json-to-messages.interval"),
      groupSize = config.get[Int]("workers.add-json-to-messages.group-size"),
      parallelism = config.get[Int]("workers.add-json-to-messages.parallelism"),
      elements = config.get[Int]("workers.add-json-to-messages.throttle.elements"),
      per = config.get[FiniteDuration]("workers.add-json-to-messages.throttle.per")
    )
}

case class WorkerSettings(
  enabled: Boolean,
  interval: FiniteDuration,
  groupSize: Int,
  parallelism: Int,
  elements: Int,
  per: FiniteDuration
)
