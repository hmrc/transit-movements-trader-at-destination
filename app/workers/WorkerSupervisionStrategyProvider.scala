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

import akka.stream.ActorAttributes
import akka.stream.Attributes
import akka.stream.Supervision
import akka.stream.Supervision.Directive
import play.api.Logger
import workers.WorkerLogKeys._
import workers.WorkerProcessingException.WorkerResumeable
import workers.WorkerProcessingException.WorkerResumeableException
import workers.WorkerProcessingException.WorkerUnresumeable
import workers.WorkerProcessingException.WorkerUnresumeableException

import scala.util.control.NonFatal

abstract private[workers] class WorkerSupervisionStrategyProvider(workerName: String, logger: Logger) {

  /**
    * The partial function that if supplied will be composed with fatalErrorSupervisionStrategy.
    * If no custom supervision strategy is provided, then the defaultSupervisorStrategy and
    * fatalErrorSupervisionStrategy will be used.
    *
    */
  def customSupervisorStrategyDecider: Option[PartialFunction[Throwable, Directive]]

  final private val fatalErrorSupervisionStrategy: PartialFunction[Throwable, Directive] = {
    case e if !NonFatal(e) =>
      logger.error(s"[$WORKER_ERROR_FATAL][$workerName] Worker saw a fatal exception and will be stopped", e)
      Supervision.Stop
  }

  final private val nonFatalSupervisionStrategy: PartialFunction[Throwable, Directive] = {
    case NonFatal(e) =>
      logger.warn(s"[$WORKER_ERROR_NONFATAL][$workerName] Worker saw this exception but will resume", e)
      Supervision.Resume
  }

  final def supervisionStrategy: Attributes = ActorAttributes.supervisionStrategy {
    customSupervisorStrategyDecider
      .map(_ orElse fatalErrorSupervisionStrategy)
      .getOrElse(nonFatalSupervisionStrategy orElse fatalErrorSupervisionStrategy)
  }

}

/**
  * A supervision strategy that resumes on non-fatal errors.
  */
class ResumeNonFatalSupervisionStrategyProvider(workerName: String, logger: Logger) extends WorkerSupervisionStrategyProvider(workerName, logger) {

  override def customSupervisorStrategyDecider: Option[PartialFunction[Throwable, Directive]] = None

}

object ResumeNonFatalSupervisionStrategyProvider {

  def apply(workerName: String, logger: Logger): ResumeNonFatalSupervisionStrategyProvider =
    new ResumeNonFatalSupervisionStrategyProvider(workerName, logger)

}

class WorkerProcessingProblemSupervisionStrategyProvider(workerName: String, logger: Logger) extends WorkerSupervisionStrategyProvider(workerName, logger) {
  import WorkerLogKeys._

  override def customSupervisorStrategyDecider: Option[PartialFunction[Throwable, Supervision.Directive]] =
    Some({
      case WorkerResumeableException(message, cause) =>
        logger.warn(s"[$WORKER_ERROR_RESUMEABLE][$workerName] $message", cause)
        Supervision.resume

      case WorkerResumeable(message) =>
        logger.info(s"[$WORKER_ERROR_RESUMEABLE][$workerName] $message")
        Supervision.resume

      case WorkerUnresumeable(message) =>
        logger.error(s"[$WORKER_ERROR_UNRESUMEABLE][$workerName] $message")
        Supervision.stop

      case WorkerUnresumeableException(message, cause) =>
        logger.error(s"[$WORKER_ERROR_UNRESUMEABLE][$workerName] $message", cause)
        Supervision.stop

      case NonFatal(e) =>
        logger.warn(s"[$WORKER_ERROR_NONFATAL][$workerName] Worker saw an exception but will resume", e)
        Supervision.resume
    })

}

object WorkerProcessingProblemSupervisionStrategyProvider {

  def apply(workerName: String, logger: Logger): WorkerProcessingProblemSupervisionStrategyProvider =
    new WorkerProcessingProblemSupervisionStrategyProvider(workerName, logger)

}
