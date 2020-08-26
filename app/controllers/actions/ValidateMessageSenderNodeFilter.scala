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

package controllers.actions
import play.api.Logger
import play.api.mvc.Results.BadRequest
import play.api.mvc._
import play.twirl.api.HtmlFormat

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class ValidateMessageSenderNodeFilter {

  def filter[R[_]](implicit ec: ExecutionContext): ActionFilter[R] = new ActionFilter[R] {

    def executionContext: ExecutionContext = ec

    override protected def filter[A](request: R[A]): Future[Option[Result]] =
      request match {
        case x: Request[A] => {
          x.body match {
            case nodeSeq: NodeSeq =>
              if ((nodeSeq \\ "MesSenMES3").isEmpty) {
                Future.successful(None)
              } else {
                Logger.warn("MesSenMES3 should not exist in body")
                Future.successful(Some(BadRequest(HtmlFormat.empty)))
              }
            case _ => {
              Logger.warn("Invalid body")
              Future.successful(Some(BadRequest(HtmlFormat.empty)))
            }
          }
        }
        case _ => Future.successful(Some(BadRequest(HtmlFormat.empty)))
      }

  }

}
