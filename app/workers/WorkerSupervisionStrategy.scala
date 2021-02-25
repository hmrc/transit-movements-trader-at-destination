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
import logging.Logging
import WorkerLogKeys._

import scala.util.control.NonFatal

trait WorkerSupervisionStrategy {
  self: Logging with ({ type W = { def workerName: String } })#W =>

  /**
    * The partial function that if supplied will be composed with fatalErrorSupervisionStrategy.
    * If no custom supervision strategy is provided, then the defaultSupervisorStrategy and
    * fatalErrorSupervisionStrategy will be used.
    *
    */
  def customSupervisionStrategy: Option[PartialFunction[Throwable, Directive]]

  final val fatalErrorSupervisionStrategy: PartialFunction[Throwable, Directive] = {
    case e if !NonFatal(e) =>
      logger.error(s"[$WORKER_ERROR_FATAL][$workerName] Worker saw a fatal exception and will be stopped", e)
      Supervision.Stop
  }

  final val defaultSupervisorStrategy: PartialFunction[Throwable, Directive] = {
    case NonFatal(e) =>
      logger.warn(s"[$WORKER_ERROR_NONFATAL][$workerName] Worker saw this exception but will resume", e)
      Supervision.Resume
  }

  final val supervisionStrategy: Attributes = ActorAttributes.supervisionStrategy {
    customSupervisionStrategy
      .map(_ orElse fatalErrorSupervisionStrategy)
      .getOrElse(defaultSupervisorStrategy orElse fatalErrorSupervisionStrategy)
  }

}
