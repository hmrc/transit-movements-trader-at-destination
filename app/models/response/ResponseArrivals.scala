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

package models.response

import models.ArrivalWithoutMessages
import play.api.libs.json.Json
import play.api.libs.json.OWrites

case class ResponseArrivals(arrivals: Seq[ResponseArrival], retrievedArrivals: Int, totalArrivals: Int, totalMatched: Int)

object ResponseArrivals {
  implicit val writes: OWrites[ResponseArrivals] = Json.writes[ResponseArrivals]

  def build(results: Seq[ArrivalWithoutMessages], totalArrivals: Int, totalMatched: Int): ResponseArrivals =
    ResponseArrivals(
      results.map(ResponseArrival.build),
      results.length,
      totalArrivals = totalArrivals,
      totalMatched = totalMatched
    )
}
