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

import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes

trait ArrivalUpdate[A] {
  def update(a: A): JsObject
}

object ArrivalUpdate {

  def apply[A: ArrivalUpdate]: ArrivalUpdate[A] = implicitly[ArrivalUpdate[A]]

  def apply[A](fn: A => JsObject): ArrivalUpdate[A] = new ArrivalUpdate[A] {
    override def update(a: A): JsObject = fn(a)
  }

  def updateModifier[A: ArrivalUpdate](a: A): JsObject = ArrivalUpdate[A].update(a)

}
