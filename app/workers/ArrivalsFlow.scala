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

import akka.NotUsed
import akka.stream.ActorAttributes
import akka.stream.Attributes
import akka.stream.Supervision
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source
import logging.Logging
import models.Arrival
import models.LockResult
import models.LockResult.LockAcquired
import repositories.ArrivalMovementRepository

import javax.inject.Inject
import scala.util.control.NonFatal

private[workers] class ArrivalsFlow @Inject()(workerConfig: WorkerConfig, arrivalMovementRepository: ArrivalMovementRepository)
    extends (() => Flow[LockResult, Arrival, NotUsed])
    with Logging {

  private val supervisionStrategy: Attributes = ActorAttributes.supervisionStrategy {
    case NonFatal(e) =>
      logger.warn("Worker saw non-fatal exception and will resume", e)
      Supervision.Resume
    case e =>
      logger.error("Worker saw a fatal exception and will be stopped", e)
      Supervision.Stop
  }

  private val settings: WorkerSettings = workerConfig.addJsonToMessagesWorkerSettings

  override def apply(): Flow[LockResult, Arrival, NotUsed] =
    Flow[LockResult]
      .filter(_ == LockAcquired)
      .flatMapConcat {
        _ =>
          Source
            .fromFutureSource(
              arrivalMovementRepository
                .arrivalsWithoutJsonMessagesSource(settings.groupSize))

      }
      .withAttributes(supervisionStrategy)

}
