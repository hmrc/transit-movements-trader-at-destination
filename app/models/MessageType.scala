/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.ReaderT

import scala.xml.NodeSeq

sealed trait MessageType extends IeMetadata {
  def code: String
  def rootNode: String
}

object MessageType extends Enumerable.Implicits {

  case object ArrivalNotification                  extends IeMetadata("IE007", "CC007A") with MessageType
  case object ArrivalRejection                     extends IeMetadata("IE008", "CC008A") with MessageType
  case object UnloadingPermission                  extends IeMetadata("IE043", "CC043A") with MessageType
  case object UnloadingRemarks                     extends IeMetadata("IE044", "CC044A") with MessageType
  case object UnloadingRemarksRejection            extends IeMetadata("IE058", "CC058A") with MessageType
  case object GoodsReleased                        extends IeMetadata("IE025", "CC025A") with MessageType
  case object XMLSubmissionNegativeAcknowledgement extends IeMetadata("IE917", "CC917A") with MessageType

  val values: Seq[MessageType] =
    Seq(
      ArrivalNotification,
      ArrivalRejection,
      UnloadingPermission,
      UnloadingRemarks,
      UnloadingRemarksRejection,
      GoodsReleased,
      XMLSubmissionNegativeAcknowledgement
    )

  def getMessageType: ReaderT[Option, NodeSeq, MessageType] =
    ReaderT[Option, NodeSeq, MessageType] {
      nodeSeq =>
        values.find(_.rootNode == nodeSeq.head.label)
    }

  implicit val ordering: Ordering[MessageType] = (x: MessageType, y: MessageType) => {
    (x, y) match {
      case (ArrivalNotification, _) => -1

      case (ArrivalRejection, ArrivalNotification) => 1
      case (ArrivalRejection, _)                   => -1

      case (XMLSubmissionNegativeAcknowledgement, ArrivalNotification) => 1
      case (XMLSubmissionNegativeAcknowledgement, UnloadingRemarks)    => 1
      case (XMLSubmissionNegativeAcknowledgement, _)                   => -1

      case (UnloadingPermission, ArrivalNotification)                  => 1
      case (UnloadingPermission, ArrivalRejection)                     => 1
      case (UnloadingPermission, XMLSubmissionNegativeAcknowledgement) => 1
      case (UnloadingPermission, _)                                    => -1

      case (UnloadingRemarks, ArrivalNotification) => 1
      case (UnloadingRemarks, UnloadingPermission) => 1
      case (UnloadingRemarks, ArrivalRejection)    => 1
      case (UnloadingRemarks, _)                   => -1

      case (UnloadingRemarksRejection, ArrivalNotification)                  => 1
      case (UnloadingRemarksRejection, UnloadingPermission)                  => 1
      case (UnloadingRemarksRejection, UnloadingRemarks)                     => 1
      case (UnloadingRemarksRejection, ArrivalRejection)                     => 1
      case (UnloadingRemarksRejection, XMLSubmissionNegativeAcknowledgement) => 1
      case (UnloadingRemarksRejection, _)                                    => -1

      case (GoodsReleased, _) => 1

      case (_, _) => -1
    }
  }

  implicit val enumerable: Enumerable[MessageType] =
    Enumerable(
      values.map(
        v => v.code -> v
      ): _*
    )
}
