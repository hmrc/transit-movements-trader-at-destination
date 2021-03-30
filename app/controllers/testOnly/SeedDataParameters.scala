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

package controllers.testOnly

import play.api.libs.json.Json

case class SeedDataParameters(
  startEori: String,
  numberOfUsers: Int,
  startMrn: String,
  movementsPerUser: Int
)

object SeedDataParameters {
  implicit val format = Json.format[SeedDataParameters]
}

//// Example Request
//{
//  startEori: "ZZ0000001",
//  numberOfUsers: 100,
//  startMrn: 2100000000,
//  movementsPerUser: 10
//}
//  // Example Response
//{
//  eoriRangeStart: "ZZ0000001",
//  eoriRangeEnd: "ZZ0000101",
//  movementsPerUser: 10,
//  startMrn: 2100000000,
//  endMrn: 2100000010,
//}
