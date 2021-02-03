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

package models

sealed case class TransitionError(reason: String)

sealed trait ArrivalStatus {
  def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus]
}

object ArrivalStatus extends Enumerable.Implicits with MongoDateTimeFormats {

  case object Initialized extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted                     => Right(ArrivalSubmitted)
      case MessageReceivedEvent.GoodsReleased                        => Right(GoodsReleased)
      case MessageReceivedEvent.UnloadingPermission                  => Right(UnloadingPermission)
      case MessageReceivedEvent.ArrivalRejected                      => Right(ArrivalRejected)
      case MessageReceivedEvent.UnloadingRemarksRejected             => Right(UnloadingRemarksRejected)
      case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement => Right(XMLSubmissionNegativeAcknowledgement)
      case _                                                         => Left(TransitionError(s"Tried to transition from Initialized to $messageReceived."))
    }
  }

  case object ArrivalSubmitted extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.ArrivalSubmitted                     => Right(ArrivalSubmitted)
      case MessageReceivedEvent.GoodsReleased                        => Right(GoodsReleased)
      case MessageReceivedEvent.UnloadingPermission                  => Right(UnloadingPermission)
      case MessageReceivedEvent.ArrivalRejected                      => Right(ArrivalRejected)
      case MessageReceivedEvent.UnloadingRemarksRejected             => Right(UnloadingRemarksRejected)
      case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement => Right(XMLSubmissionNegativeAcknowledgement)
      case _                                                         => Left(TransitionError(s"Tried to transition from ArrivalSubmitted to $messageReceived."))
    }
  }

  case object UnloadingPermission extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.UnloadingPermission       => Right(UnloadingPermission)
      case MessageReceivedEvent.UnloadingRemarksSubmitted => Right(UnloadingRemarksSubmitted)
      case MessageReceivedEvent.GoodsReleased             => Right(GoodsReleased)
      case _                                              => Left(TransitionError(s"Tried to transition from ArrivalSubmitted to $messageReceived."))
    }
  }

  case object GoodsReleased extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = Right(this)
  }

  case object ArrivalRejected extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.ArrivalRejected => Right(ArrivalRejected)
      case MessageReceivedEvent.GoodsReleased   => Right(GoodsReleased)
      case _                                    => Left(TransitionError(s"Tried to transition from ArrivalRejected to $messageReceived."))
    }
  }

  case object XMLSubmissionNegativeAcknowledgement extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement => Right(XMLSubmissionNegativeAcknowledgement)
      case _                                                         => Left(TransitionError(s"Tried to transition from XMLSubmissionNegativeAcknowledgement to $messageReceived."))
    }
  }

  case object UnloadingRemarksSubmitted extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.UnloadingRemarksSubmitted => Right(UnloadingRemarksSubmitted)
      case MessageReceivedEvent.UnloadingRemarksRejected  => Right(UnloadingRemarksRejected)
      case MessageReceivedEvent.GoodsReleased             => Right(GoodsReleased)
      case _                                              => Left(TransitionError(s"Tried to transition from UnloadingRemarksSubmitted to $messageReceived."))
    }
  }

  case object UnloadingRemarksRejected extends ArrivalStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, ArrivalStatus] = messageReceived match {
      case MessageReceivedEvent.UnloadingRemarksRejected  => Right(UnloadingRemarksRejected)
      case MessageReceivedEvent.UnloadingRemarksSubmitted => Right(UnloadingRemarksSubmitted)
      case MessageReceivedEvent.GoodsReleased             => Right(GoodsReleased)
      case _                                              => Left(TransitionError(s"Tried to transition from UnloadingRemarksRejected to $messageReceived."))
    }
  }

  val values = Seq(
    Initialized,
    ArrivalSubmitted,
    UnloadingPermission,
    UnloadingRemarksSubmitted,
    GoodsReleased,
    ArrivalRejected,
    UnloadingRemarksRejected,
    XMLSubmissionNegativeAcknowledgement
  )

  implicit val enumerable: Enumerable[ArrivalStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)

}
