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

package workers

import akka.NotUsed
import akka.stream.ActorAttributes
import akka.stream.Attributes
import akka.stream.Materializer
import akka.stream.Supervision
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.SinkQueueWithCancel
import akka.stream.scaladsl.Source
import logging.Logging
import models.Arrival
import repositories.ArrivalMovementRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

class AddJsonToMessagesWorker @Inject()(
  workerConfig: WorkerConfig,
  arrivalMovementRepository: ArrivalMovementRepository,
  addJsonToMessagesTransformer: AddJsonToMessagesTransformer,
  workerLockingService: WorkerLockingService
)(implicit ec: ExecutionContext, m: Materializer)
    extends Logging {

  private val supervisionStrategy: Attributes = ActorAttributes.supervisionStrategy {
    case NonFatal(e) =>
      logger.warn("Worker saw this exception but will resume", e)
      Supervision.resume
    case _ =>
      logger.error("Worker saw a fatal exception and will be stopped")
      Supervision.stop
  }

  private val settings = workerConfig.addJsonToMessagesWorkerSettings

  val tap: SinkQueueWithCancel[Seq[(Arrival, NotUsed)]] = {

    logger.info("Worker started")

    Source
      .fromIterator(
        () => workerLockingService
      )
      .throttle(1, settings.interval)
      .mapAsync(1)(identity)
      .via((new ArrivalsFlow(workerConfig, arrivalMovementRepository))())
      .via(addJsonToMessagesTransformer.flow)
      .mapAsync(1)(
        x =>
          workerLockingService
            .releaseLock()
            .map(
              _ => x
          )
      )
      .withAttributes(supervisionStrategy)
      .wireTapMat(Sink.queue())(Keep.right)
      .to(Sink.ignore)
      .run()
  }
}
