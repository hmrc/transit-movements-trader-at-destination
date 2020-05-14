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

package config

import javax.inject.Inject
import play.api.Logger
import play.api.http.HttpErrorHandler
import play.api.mvc.RequestHeader
import play.api.mvc.Result
import play.api.mvc.Results._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ErrorHandler @Inject()()(implicit ec: ExecutionContext) extends HttpErrorHandler {
  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    Logger.warn(s"[ErrorHandler][onClientError], error for (${request.method}) [${request.uri}] with status: $statusCode and message: $message")
    Future.successful(Status(statusCode)(message))
  }

  override def onServerError(request: RequestHeader, ex: Throwable): Future[Result] = {
    Logger.warn(s"[ErrorHandler][onServerError], error for (${request.method}) [${request.uri}] with error: ${ex.getStackTrace}")
    Future.successful(InternalServerError("A server error occurred: " + ex.getMessage))
  }
}
