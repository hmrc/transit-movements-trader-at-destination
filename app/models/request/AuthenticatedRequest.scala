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

package models.request

import models.Arrival
import models.ArrivalMessages
import models.ArrivalWithoutMessages
import models.ChannelType
import models.EnrolmentId
import play.api.mvc.Request
import play.api.mvc.WrappedRequest

case class AuthenticatedRequest[A](request: Request[A], channel: ChannelType, enrolmentId: EnrolmentId) extends WrappedRequest[A](request) {

  private def matchesEnrolmentId(eoriNumber: String): Boolean =
    enrolmentId.value.fold(
      vatReg => eoriNumber == vatReg.value,
      eori => eoriNumber == eori.value,
      (vatReg, eori) =>
        eoriNumber == vatReg.value ||
          eoriNumber == eori.value
    )

  def hasMatchingEnrolmentId(arrival: Arrival): Boolean =
    matchesEnrolmentId(arrival.eoriNumber)

  def hasMatchingEnrolmentId(arrival: ArrivalWithoutMessages): Boolean =
    matchesEnrolmentId(arrival.eoriNumber)

  def hasMatchingEnrolmentId(arrival: ArrivalMessages): Boolean =
    matchesEnrolmentId(arrival.eoriNumber)

  lazy val length: Int = headers.get(play.api.http.HeaderNames.CONTENT_LENGTH).get.toInt
}
