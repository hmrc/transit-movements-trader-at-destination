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

package utils

import play.api.libs.json.Json

case class ErrorResponse(message: String)

object ErrorResponse {
  implicit val formats = Json.format[ErrorResponse]
}

object ErrorResponseBuilder {

  val failedSavingToDatabase: ErrorResponse = {
    ErrorResponse("failed to save an Arrival Notification to Database")
  }

  val failedSubmissionToEIS: ErrorResponse = {
    ErrorResponse("failed submission to EIS")
  }

  val failedSavingArrivalNotification: ErrorResponse = {
    ErrorResponse("failed to save an Arrival Notification to Database")
  }

  def failedXmlValidation(reason: String): ErrorResponse =
    ErrorResponse(s"Xml validation failed for the following reason: $reason")

  val failedXmlConversion: ErrorResponse = {
    ErrorResponse("failed to convert to xml")
  }

  val failedToWrapXml: ErrorResponse = {
    ErrorResponse("failed to wrap xml in transit wrapper")
  }

  val failedToCreateRequestModel: ErrorResponse = {
    ErrorResponse("could not create request model")
  }

  val failedToCreateInterchangeControlRef: ErrorResponse = {
    ErrorResponse("failed to create InterchangeControlReference")
  }
}
