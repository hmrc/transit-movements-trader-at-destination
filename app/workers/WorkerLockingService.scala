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

import com.google.inject.ImplementedBy
import logging.Logging
import models.LockResult
import play.api.inject.ApplicationLifecycle
import repositories.WorkerLockRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[WorkerLockingServiceImpl])
private[workers] trait WorkerLockingService extends Iterator[Future[LockResult]] {

  def releaseLock(): Future[Boolean]

}

private[workers] class WorkerLockingServiceImpl @Inject()(
  workerConfig: WorkerConfig,
  workerLockRepository: WorkerLockRepository,
  lifecycle: ApplicationLifecycle
)(implicit ec: ExecutionContext)
    extends WorkerLockingService
    with Logging {

  private val lockId = "add-json-to-messages-worker"

  private val settings = workerConfig.addJsonToMessagesWorkerSettings

  private var workerEnabled = settings.enabled

  override def hasNext: Boolean = workerEnabled

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

  lifecycle.addStopHook(() => {
    workerEnabled = false
    Future.successful(())
  })

  def releaseLock(): Future[Boolean] =
    workerLockRepository.unlock(lockId).map {
      result =>
        logger.info(s"Released lock $lockId")
        result
    }
}
