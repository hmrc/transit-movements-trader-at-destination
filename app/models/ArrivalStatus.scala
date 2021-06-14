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

sealed abstract class ArrivalStatus extends Product with Serializable

object ArrivalStatus extends Enumerable.Implicits with MongoDateTimeFormats {

  case object Initialized extends ArrivalStatus

  case object ArrivalSubmitted extends ArrivalStatus

  case object UnloadingPermission extends ArrivalStatus

  case object GoodsReleased extends ArrivalStatus

  case object ArrivalRejected extends ArrivalStatus

  case object ArrivalXMLSubmissionNegativeAcknowledgement extends ArrivalStatus

  case object UnloadingRemarksXMLSubmissionNegativeAcknowledgement extends ArrivalStatus

  case object UnloadingRemarksSubmitted extends ArrivalStatus

  case object UnloadingRemarksRejected extends ArrivalStatus

  val values = Seq(
    Initialized,
    ArrivalSubmitted,
    UnloadingPermission,
    UnloadingRemarksSubmitted,
    GoodsReleased,
    ArrivalRejected,
    UnloadingRemarksRejected,
    ArrivalXMLSubmissionNegativeAcknowledgement,
    UnloadingRemarksXMLSubmissionNegativeAcknowledgement
  )

  implicit val enumerable: Enumerable[ArrivalStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)

}
