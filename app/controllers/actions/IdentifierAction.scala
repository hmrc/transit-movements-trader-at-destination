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

import com.google.inject.Inject
import config.AppConfig
import models.request.IdentifierRequest
import play.api.Logger
import play.api.mvc.Results.Unauthorized
import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait IdentifierAction extends ActionBuilder[IdentifierRequest, AnyContent] with ActionFunction[Request, IdentifierRequest]

class AuthenticatedIdentifierAction @Inject()(
  override val authConnector: AuthConnector,
  config: AppConfig,
  val parser: BodyParsers.Default
)(implicit val executionContext: ExecutionContext)
    extends IdentifierAction
    with AuthorisedFunctions {

  private val enrolmentIdentifierKey: String = "VATRegNoTURN"

  override def invokeBlock[A](request: Request[A], block: IdentifierRequest[A] => Future[Result]): Future[Result] = {

    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers)

    authorised(Enrolment(config.enrolmentKey)).retrieve(Retrievals.authorisedEnrolments) {
      enrolments =>
        val eoriNumber: String = (for {
          enrolment  <- enrolments.enrolments.find(_.key.equals(config.enrolmentKey))
          identifier <- enrolment.getIdentifier(enrolmentIdentifierKey)
        } yield identifier.value).getOrElse(throw InsufficientEnrolments(s"Unable to retrieve enrolment for $enrolmentIdentifierKey"))

        block(IdentifierRequest(request, eoriNumber))
    }
  }.recover {
    case e: AuthorisationException => {
      //TODO: Alerting - We need to clarify the logging and alerting for the failures below
      Logger.warn(s"Failed to authorise with the following exception: $e")
      Unauthorized
    }
  }
}