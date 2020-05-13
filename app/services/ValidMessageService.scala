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

import cats.data.NonEmptyList
import models.Arrival
import models.MessageType
import models.MovementMessage
import models.MovementMessageWithStatus
import models.MovementMessageWithoutStatus
import models.MessageType._

class ValidMessageService {

  def arrivalNotification(arrival: Arrival): MovementMessage = arrival.messages match {
    case NonEmptyList(arrivalNotification, Nil)      => arrivalNotification
    case NonEmptyList(arrivalNotification, _ :: Nil) => arrivalNotification
    case a =>
      a.foldLeft(Seq.empty[MovementMessageWithStatus]) {
          case (acc, m @ MovementMessageWithStatus(_, ArrivalNotification, _, _, _)) => acc :+ m
          case (acc, _)                                                              => acc
        }
        .maxBy(_.messageCorrelationId)
  }

  def arrivalRejection(arrival: Arrival): Option[MovementMessage] = {

    def sameNumber(msgs: NonEmptyList[MovementMessage]): Boolean = {
      val numIe008 = msgs.toList.count {
        case MovementMessageWithoutStatus(_, ArrivalRejection, _, _) => true
        case _                                                       => false
      }
      val numIe007 = msgs.toList.count {
        case MovementMessageWithStatus(_, ArrivalNotification, _, _, _) => true
        case _                                                          => false
      }

      numIe007 == numIe008
    }

    val ie008Messages = arrival.messages
      .foldLeft(Seq.empty[MovementMessageWithoutStatus]) {
        case (acc, m @ MovementMessageWithoutStatus(_, ArrivalRejection, _, _)) => acc :+ m
        case (acc, _)                                                           => acc
      }

    if (ie008Messages.nonEmpty && sameNumber(arrival.messages)) Some(ie008Messages.maxBy(_.messageCorrelationId))
    else None
  }

}
