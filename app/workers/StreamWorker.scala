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
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.RunnableGraph
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.SinkQueueWithCancel
import akka.stream.scaladsl.Source
import logging.Logging
import models.LockResult
import models.LockResult.LockAcquired
import play.api.Configuration

import scala.concurrent.ExecutionContext

/**
  *
  * This worker coordinates among multiple instance for the same work associated with a workerName, an ensures that
  * a only a single instance is working at a time.
  *
  * The stream provides a LockResult and only if the there is [[models.LockResult.LockAcquired]] will the flow be run.
  *
  * @param workerName The worker's name that will be used for the lock identifier, to load the config, and is added to logging messages
  * @param config The worker config is read using the workerName
  * @param workerLockingService Locks work to this single instance of the worker when there are multiple application instances
  * @tparam A The type of the out value of the `flow`
  */
abstract class StreamWorker[A](
  workerLockingService: WorkerLockingService
)(val workerName: String, config: Configuration)(implicit materializer: Materializer)
    extends WorkerSupervisionStrategy
    with Logging {

  final val workerSettings @ WorkerSettings(enabled, interval, groupSize, parallelism, elements, per) =
    config.get[WorkerSettings](s"""workers.$workerName""")

  final val source: Source[LockResult, NotUsed] =
    Source
      .fromIterator(() => workerLockingService)
      .throttle(1, interval)
      .mapAsync(1)(identity)
      .filter(_ == LockAcquired)

  def flow: Flow[LockResult, A, NotUsed]

  final val sink: Sink[A, NotUsed] =
    Flow[A]
      .map(_ => workerLockingService.releaseLock())
      .to(Sink.ignore)
      .mapMaterializedValue(_ => NotUsed)

  private val sourceAndFlow: Source[A, NotUsed] =
    source
      .via(flow)

  val runWithTap: RunnableGraph[SinkQueueWithCancel[A]] =
    sourceAndFlow
      .wireTapMat(Sink.queue())(Keep.right)
      .to(sink)

  val running: Boolean = if (enabled) {
    logger.info(s"[WORKER_STARTED][$workerName]")
    sourceAndFlow
      .to(sink)
      .run()
    true
  } else {
    logger.info(s"[WORKER_DISABLED][$workerName]")
    false
  }

}

object WorkerLogKeys {

  val WORKER_STARTED            = "WORKER_STARTED"
  val WORKER_DISABLED           = "WORKER_DISABLED"
  val WORKER_ERROR_RESUMEABLE   = "WORKER_ERROR_RESUMEABLE"
  val WORKER_ERROR_UNRESUMEABLE = "WORKER_ERROR_UNRESUMEABLE"
  val WORKER_ERROR_FATAL        = "WORKER_ERROR_FATAL"
  val WORKER_ERROR_NONFATAL     = "WORKER_ERROR_NONFATAL"
  val WORKER_LOG                = "WORKER_LOG"

}
