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
import akka.stream.Materializer
import akka.stream.Supervision
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.SinkQueueWithCancel
import akka.stream.scaladsl.Source
import cats.data.NonEmptyList
import logging.Logging
import models.LockResult.LockAcquired
import models.Arrival
import models.LockResult
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import repositories.LockRepository
import repositories.ArrivalMovementRepository
import repositories.WorkerLockRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

class AddJsonToMessagesWorker @Inject()(
  workerConfig: WorkerConfig,
  workerLockRepository: WorkerLockRepository,
  arrivalMovementRepository: ArrivalMovementRepository,
  addJsonToMessagesTransformer: AddJsonToMessagesTransformer
)(implicit ec: ExecutionContext, m: Materializer)
    extends Logging {

  private val supervisionStrategy: Supervision.Decider = {
    case NonFatal(e) =>
      logger.warn("Worker saw this exception but will resume", e)
      Supervision.resume
    case _ =>
      logger.error("Worker saw a fatal exception and will be stopped")
      Supervision.stop
  }

  private val settings = workerConfig.addJsonToMessagesWorkerSettings
  private val lockId   = "add-json-to-messages-worker"

  private def releaseLock(): Future[Boolean] =
    workerLockRepository.unlock(lockId).map {
      result =>
        logger.info(s"Released lock $lockId")
        result
    }

  private val lockProvider: Iterator[Future[LockResult]] = new Iterator[Future[LockResult]] {
    override def hasNext: Boolean = settings.enabled

    override def next(): Future[LockResult] =
      if (hasNext) {
        logger.info(s"Attempting to acquire lock $lockId")

        workerLockRepository.lock(lockId).map {
          result =>
            logger.info(s"Result of attempting to acquire lock $lockId: $result")
            result
        }
      } else {
        throw new NoSuchElementException("This worker is disabled in configuration, so there are no more locks to be had")
      }
  }

  private def runBatch(batch: Seq[Arrival]): Future[Seq[(Arrival, NotUsed)]] =
    Source
      .fromIterator(() => batch.iterator)
      .throttle(settings.elements, settings.per)
      .mapAsync(settings.parallelism)(addJsonToMessagesTransformer.run)
      .grouped(settings.groupSize)
      .wireTapMat(Sink.queue())(Keep.right)
      .to(Sink.ignore)
      .withAttributes(ActorAttributes.supervisionStrategy(supervisionStrategy))
      .run()
      .pull()
      .map(_.getOrElse(Nil))
      .map(Seq.apply)

  val tap: SinkQueueWithCancel[Seq[(Arrival, NotUsed)]] = {

    logger.info("Worker started")

    Source
      .fromIterator(() => lockProvider)
      .throttle(1, settings.interval)
      .mapAsync(1)(identity)
      .filter(_ == LockAcquired)
      .mapAsync(1)(_ => arrivalMovementRepository.arrivalsWithoutJsonMessages(settings.groupSize))
      .mapAsync(1)(runBatch)
      .mapAsync(1)(x => releaseLock().map(_ => x))
      .wireTapMat(Sink.queue())(Keep.right)
      .to(Sink.ignore)
      .withAttributes(ActorAttributes.supervisionStrategy(supervisionStrategy))
      .run()
  }
}
