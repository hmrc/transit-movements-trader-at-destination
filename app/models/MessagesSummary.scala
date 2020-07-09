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

import play.api.libs.json._

case class MessagesSummary(arrival: Arrival,
                           arrivalNotification: MessageId,
                           arrivalRejection: Option[MessageId],
                           unloadingPermission: Option[MessageId] = None,
                           unloadingRemarks: Option[MessageId] = None,
                           unloadingRemarksRejection: Option[MessageId] = None)

object MessagesSummary {

  implicit val writes: OWrites[MessagesSummary] =
    OWrites[MessagesSummary] {
      case MessagesSummary(arrival, arrivalNotification, arrivalRejection, unloadingPermission, unloadingRemarks, unloadingRemarksRejection) =>
        Json
          .obj(
            "arrivalId" -> arrival.arrivalId,
            "messages" -> Json.obj(
              MessageType.ArrivalNotification.code -> controllers.routes.MessagesController.getMessage(arrival.arrivalId, arrivalNotification).url,
              MessageType.ArrivalRejection.code    -> arrivalRejection.map(controllers.routes.MessagesController.getMessage(arrival.arrivalId, _).url),
              MessageType.UnloadingPermission.code -> unloadingPermission.map(controllers.routes.MessagesController.getMessage(arrival.arrivalId, _).url),
              MessageType.UnloadingRemarks.code    -> unloadingRemarks.map(controllers.routes.MessagesController.getMessage(arrival.arrivalId, _).url),
              MessageType.UnloadingRemarksRejection.code -> unloadingRemarksRejection.map(
                controllers.routes.MessagesController.getMessage(arrival.arrivalId, _).url)
            )
          )
          .filterNulls

    }
}
