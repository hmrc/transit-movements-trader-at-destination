/*
 * Copyright 2019 HM Revenue & Customs
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

package models.messages

import generators.MessageGenerators
import models.ProcedureType
import models.behaviours.JsonBehaviours
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsError
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json

class ArrivalNotificationSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with MessageGenerators with JsonBehaviours {

  "Normal notification" - {
    "must deserialise" in {
      forAll(arbitrary[NormalNotification]) {
        normalNotification =>
          val json = createNormalNotificationJson(normalNotification)
          json.validate[NormalNotification] mustEqual JsSuccess(normalNotification)
      }
    }

    "must fail to deserialise when `procedure` is `simplified`" in {
      forAll(arbitrary[NormalNotification]) {
        normalNotification =>
          val jsonWithSimplified = {
            createNormalNotificationJson(normalNotification) ++ Json.obj("procedure" -> Json.toJson(ProcedureType.Simplified))
          }

          jsonWithSimplified.validate[NormalNotification] mustEqual JsError("procedure must be `normal`")
      }
    }

    "must serialise" in {
      forAll(arbitrary[NormalNotification]) {
        normalNotification =>
          val json = createNormalNotificationJson(normalNotification)
          Json.toJson(normalNotification)(NormalNotification.writes) mustEqual json
      }
    }
  }

  "Simplified notification" - {
    "must deserialise" in {
      forAll(arbitrary[SimplifiedNotification]) {
        simplifiedNotification =>
          val json = createSimplifiedNotificationJson(simplifiedNotification)
          json.validate[SimplifiedNotification] mustEqual JsSuccess(simplifiedNotification)
      }
    }

    "must fail to deserialise when `procedure` is `normal`" in {
      forAll(arbitrary[SimplifiedNotification]) {
        simplifiedNotification =>
          val jsonWithNormal = {
            createSimplifiedNotificationJson(simplifiedNotification) ++ Json.obj("procedure" -> Json.toJson(ProcedureType.Normal))
          }

          jsonWithNormal.validate[SimplifiedNotification] mustEqual JsError("procedure must be `simplified`")
      }
    }

    "must serialise" in {
      forAll(arbitrary[SimplifiedNotification]) {
        simplifiedNotification =>
          val json = createSimplifiedNotificationJson(simplifiedNotification)
          Json.toJson(simplifiedNotification)(SimplifiedNotification.writes) mustEqual json
      }
    }
  }

  "Arrival Notification" - {
    "must deserialise to a Normal notification" in {
      forAll(arbitrary[NormalNotification]) {
        normalNotification =>
          val json = createNormalNotificationJson(normalNotification)
          json.validate[ArrivalNotification] mustEqual JsSuccess(normalNotification)
      }
    }

    "must deserialise to a Simplified notification" in {
      forAll(arbitrary[SimplifiedNotification]) {
        simplifiedNotification =>
          val json = createSimplifiedNotificationJson(simplifiedNotification)
          json.validate[ArrivalNotification] mustEqual JsSuccess(simplifiedNotification)
      }
    }

    "must serialise from a Normal notification" in {
      forAll(arbitrary[NormalNotification]) {
        normalNotification =>
          val json = createNormalNotificationJson(normalNotification)
          Json.toJson(normalNotification: ArrivalNotification) mustEqual json
      }
    }

    "must serialise from a Simplified notification" in {
      forAll(arbitrary[SimplifiedNotification]) {
        simplifiedNotification =>
          val json = createSimplifiedNotificationJson(simplifiedNotification)
          Json.toJson(simplifiedNotification: ArrivalNotification) mustEqual json
      }
    }
  }

  private def createNormalNotificationJson(notification: NormalNotification): JsObject =
    Json.obj(
      "procedure"               -> notification.procedure,
      "movementReferenceNumber" -> notification.movementReferenceNumber,
      "notificationPlace"       -> notification.notificationPlace,
      "notificationDate"        -> notification.notificationDate
    ) ++ {
      notification.customsSubPlace match {
        case Some(subPlace) => Json.obj("customsSubPlace" -> Json.toJson(subPlace))
        case _              => JsObject.empty
      }
    } ++ Json.obj("trader" -> Json.toJson(notification.trader), "presentationOffice" -> notification.presentationOffice) ++ {
      notification.enRouteEvents match {
        case Some(enRouteEvents) => Json.obj("enRouteEvents" -> Json.toJson(notification.enRouteEvents))
        case _                   => JsObject.empty
      }
    }

  private def createSimplifiedNotificationJson(notification: SimplifiedNotification): JsObject =
    Json.obj(
      "procedure"               -> notification.procedure,
      "movementReferenceNumber" -> notification.movementReferenceNumber,
      "notificationPlace"       -> notification.notificationPlace,
      "notificationDate"        -> notification.notificationDate
    ) ++ {
      notification.approvedLocation match {
        case Some(approvedLocation) => Json.obj("approvedLocation" -> approvedLocation)
        case _                      => JsObject.empty
      }
    } ++ Json.obj("trader" -> Json.toJson(notification.trader), "presentationOffice" -> notification.presentationOffice) ++ {
      notification.enRouteEvents match {
        case Some(enrouteEvents) => Json.obj("enRouteEvents" -> Json.toJson(notification.enRouteEvents))
        case _                   => JsObject.empty
      }
    }
}
