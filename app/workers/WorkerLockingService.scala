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

import logging.Logging
import models.LockResult
import repositories.WorkerLockRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[workers] class WorkerLockingService @Inject()(
  workerConfig: WorkerConfig,
  workerLockRepository: WorkerLockRepository
)(implicit ec: ExecutionContext)
    extends Iterator[Future[LockResult]]
    with Logging {

  private val lockId = "add-json-to-messages-worker"

  private val settings = workerConfig.addJsonToMessagesWorkerSettings

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

  def releaseLock(): Future[Boolean] =
    workerLockRepository.unlock(lockId).map {
      result =>
        logger.info(s"Released lock $lockId")
        result
    }
}
