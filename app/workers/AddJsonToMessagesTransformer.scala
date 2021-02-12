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
import cats.data.NonEmptyList
import logging.Logging
import models.Arrival
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import repositories.ArrivalMovementRepository
import repositories.LockRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

private[workers] class AddJsonToMessagesTransformer @Inject()(
  workerConfig: WorkerConfig,
  arrivalMovementRepository: ArrivalMovementRepository,
  arrivalLockRepository: LockRepository
)(implicit ec: ExecutionContext)
    extends Logging {

  private val settings = workerConfig.addJsonToMessagesWorkerSettings

  private val supervisionStrategy: Attributes = ActorAttributes.supervisionStrategy {
    case NonFatal(e) =>
      logger.warn("Worker saw this exception but will resume", e)
      Supervision.resume
    case _ =>
      logger.error("Worker saw a fatal exception and will be stopped")
      Supervision.stop
  }

  val flow: Flow[Arrival, Seq[(Arrival, NotUsed)], NotUsed] =
    Flow[Arrival]
      .throttle(settings.elements, settings.per)
      .mapAsync(settings.parallelism)(run)
      .grouped(settings.groupSize)
      .withAttributes(supervisionStrategy)

  def run(arrival: Arrival): Future[(Arrival, NotUsed)] =
    arrivalLockRepository.lock(arrival.arrivalId).flatMap {
      case true =>
        logger.info(s"Adding JSON to messages on arrival ${arrival.arrivalId.index}")

        val updatedMessages: NonEmptyList[MovementMessage] = arrival.messages.map {
          case m: MovementMessageWithoutStatus =>
            MovementMessageWithoutStatus(m.dateTime, m.messageType, m.message, m.messageCorrelationId)
          case m: MovementMessageWithStatus =>
            MovementMessageWithStatus(m.dateTime, m.messageType, m.message, m.status, m.messageCorrelationId)
        }

        arrivalMovementRepository.resetMessages(arrival.arrivalId, updatedMessages).flatMap {
          _ =>
            arrivalLockRepository.unlock(arrival.arrivalId).map {
              _ =>
                (arrival, NotUsed)
            }
        } recoverWith {
          case e: Throwable =>
            logger.error(s"Received an error trying to reset messages for arrival s${arrival.arrivalId.index}", e)

            arrivalLockRepository.unlock(arrival.arrivalId).map {
              _ =>
                (arrival, NotUsed)
            }
        }

      case false =>
        logger.info(s"Arrival ${arrival.arrivalId} is locked, so messages will not be updated")
        Future.successful((arrival, NotUsed))
    }

}
