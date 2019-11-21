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

package controllers

import connectors.MessageConnector
import javax.inject.Inject
import models.WebChannel
import models.messages.ArrivalNotification
import models.messages.request.ArrivalNotificationRequest
import play.api.libs.json.{JsError, Reads}
import play.api.mvc._
import repositories.ArrivalNotificationRepository
import services.{FailedCreatingInterchangeControlReference, InterchangeControlReferenceService, XmlSubmissionService}
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Utility.trim

class ArrivalNotificationController @Inject()(
                                               cc: ControllerComponents,
                                               service: XmlSubmissionService,
                                               bodyParsers: PlayBodyParsers,
                                               interchangeControlReferenceService: InterchangeControlReferenceService,
                                               arrivalNotificationRepository: ArrivalNotificationRepository,
                                               messageConnector: MessageConnector
                                             )
  extends BackendController(cc) {

  /**
    * TODO: -
    * Should nextInterchangeControlReferenceId be within it's own service? persistToMongo could also be accessed through it?
    * Should we use MessageConnector directly or inject a service?
    * SubmissionService - does this need renaming?
    * Change saving to mongo order
    */

  def validateJson[A: Reads]: BodyParser[A] = bodyParsers.json.validate(
    _.validate[A].asEither.left.map(e =>
      BadRequest(JsError.toJson(e)).as("application/json")
    ))

  def post(): Action[ArrivalNotification] = Action.async(validateJson[ArrivalNotification]) {
    implicit request =>

      interchangeControlReferenceService.getInterchangeControlReferenceId.flatMap {
        case Right(interchangeControlReferenceId) =>
          service buildAndValidateXml(request.body, interchangeControlReferenceId) match {
            case Right(xml) =>
              service.saveAndSubmitXml(xml, ArrivalNotificationRequest.messageCode, WebChannel, request.body)
            case Left(error) =>
              Future.successful(BadRequest(error.toString))
          }
        case Left(FailedCreatingInterchangeControlReference) => {
          Future.successful(InternalServerError)
        }
      }
  }
}