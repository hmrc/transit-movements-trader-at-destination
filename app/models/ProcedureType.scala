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

import play.api.libs.json.JsError
import play.api.libs.json.JsString
import play.api.libs.json.JsSuccess
import play.api.libs.json.Reads
import play.api.libs.json.Writes

sealed trait ProcedureType

object ProcedureType {

  case object Normal extends ProcedureType {
    override def toString: String = "normal"
  }

  case object Simplified extends ProcedureType {
    override def toString: String = "simplified"
  }

  implicit lazy val reads: Reads[ProcedureType] = Reads {
    case JsString("normal")     => JsSuccess(Normal)
    case JsString("simplified") => JsSuccess(Simplified)
    case _                      => JsError("Unknown procedure type")
  }

  implicit lazy val writes: Writes[ProcedureType] = Writes {
    case Normal     => JsString("normal")
    case Simplified => JsString("simplified")
  }
}
