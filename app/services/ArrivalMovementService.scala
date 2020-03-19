/*
 * Copyright 2020 HM Revenue & Customs
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

package services

import java.time.LocalDate
import java.time.LocalTime

import cats._
import cats.data._
import cats.implicits._
import com.google.inject.Inject
import models.{Arrival, ArrivalMovement, MessageType, TimeStampedMessageXml}
import models.messages.MovementReferenceNumber
import repositories.ArrivalIdRepository
import utils.Format

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import scala.xml.NodeSeq

class ArrivalMovementService @Inject()(arrivalIdRepository: ArrivalIdRepository)(implicit ec: ExecutionContext) {
  import ArrivalMovementService._

  def makeArrivalMovement(eori: String): Reader[NodeSeq, Option[Future[Arrival]]] =
    for {
      correctRoot <- correctRootNodeR
      date        <- dateOfPrepR
      time        <- timeOfPrepR
      mrn         <- mrnR
      xmlMessage  <- Reader(identity[NodeSeq])
    } yield {

      for {
        _ <- correctRoot
        d <- date
        t <- time
        m <- mrn.map(_.value)
      } yield {
        arrivalIdRepository
          .nextId()
          .map(Arrival(_, m, eori, Seq(TimeStampedMessageXml(d, t, xmlMessage))))
      }
    }
}

object ArrivalMovementService {

  def correctRootNodeR: Reader[NodeSeq, Option[Unit]] =
    Reader[NodeSeq, Option[Unit]] {
      nodeSeq =>
        if (nodeSeq.head.label == MessageType.ArrivalNotification.rootNode) Some(()) else None
    }

  val dateOfPrepR: Reader[NodeSeq, Option[LocalDate]] =
    Reader[NodeSeq, NodeSeq](_ \ "DatOfPreMES9")
      .map(_.text)
      .map(Try(_))
      .map(_.map(LocalDate.parse(_, Format.dateFormatter)))
      .map(_.toOption) // TODO: We are not propagating this failure back, do we need to do this?

  val timeOfPrepR: Reader[NodeSeq, Option[LocalTime]] =
    Reader[NodeSeq, NodeSeq](_ \ "TimOfPreMES10")
      .map(_.text)
      .map(Try(_))
      .map(_.map(LocalTime.parse(_, Format.timeFormatter)))
      .map(_.toOption) // TODO: We are not propagating this failure back, do we need to do this?

  val mrnR: Reader[NodeSeq, Option[MovementReferenceNumber]] =
    Reader[NodeSeq, NodeSeq](_ \ "HEAHEA" \ "DocNumHEA5")
      .map(_.text)
      .map {
        case mrnString if !mrnString.isEmpty => Some(MovementReferenceNumber(mrnString))
        case _                               => None
      }
}
