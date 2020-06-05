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

package models

import cats.Semigroup
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes

sealed trait UpdateSpike
case class AStatusUpdate(arrivalStatus: ArrivalStatus) extends UpdateSpike

case class MStatusUpdate(messageId: MessageId, messageStatus: MessageStatus) extends UpdateSpike

object MStatusUpdate {
  implicit def mStatusUpdateUpdate(implicit writes: Writes[MessageStatus]): ArrivalModifier[MStatusUpdate] =
    ArrivalModifier(
      value =>
        Json.obj(
          "$set" ->
            Json.obj(
              s"messages.${value.messageId.index}.status" -> value.messageStatus
            )
      )
    )
}

case class ApMStatusUpdate(arrivalStatus: ArrivalStatus, messageId: MessageId, messageStatus: MessageStatus) extends UpdateSpike

object UpdateSpike {

  implicit object UpdateSpikeSemiGroupInstance extends Semigroup[UpdateSpike] {
    override def combine(x: UpdateSpike, y: UpdateSpike): UpdateSpike =
      (x, y) match {
        case (AStatusUpdate(_), x @ AStatusUpdate(_))         => x
        case (AStatusUpdate(aSt), MStatusUpdate(mId, mSt))    => ApMStatusUpdate(aSt, mId, mSt)
        case (AStatusUpdate(_), x @ ApMStatusUpdate(_, _, _)) => x

        case (MStatusUpdate(_, _), x @ MStatusUpdate(_, _))      => x
        case (MStatusUpdate(mId, mSt), AStatusUpdate(aSt))       => ApMStatusUpdate(aSt, mId, mSt)
        case (MStatusUpdate(_, _), x @ ApMStatusUpdate(_, _, _)) => x

        case (ApMStatusUpdate(_, _, _), x @ ApMStatusUpdate(_, _, _)) => x
        case (ApMStatusUpdate(_, mId, mSt), AStatusUpdate(a))         => ApMStatusUpdate(a, mId, mSt)
        case (ApMStatusUpdate(aSt, _, _), MStatusUpdate(mId, mSt))    => ApMStatusUpdate(aSt, mId, mSt)

      }
  }

  implicit object UpdateSpikeArrivalUpdate extends ArrivalModifier[UpdateSpike] {
    override def toJson(a: UpdateSpike): JsObject =
      a match {
        case AStatusUpdate(arrivalStatus) => ArrivalModifier[ArrivalStatus].toJson(arrivalStatus)
        case m @ MStatusUpdate(_, _)      => ArrivalModifier[MStatusUpdate].toJson(m)
        case ApMStatusUpdate(aSt, mId, mSt) =>
          ArrivalModifier[ArrivalStatus].toJson(aSt) deepMerge
            ArrivalModifier[MStatusUpdate].toJson(MStatusUpdate(mId, mSt))
      }
  }

}
