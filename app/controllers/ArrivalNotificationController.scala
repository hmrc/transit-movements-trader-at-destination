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

import java.time.LocalDateTime

import config.AppConfig
import connectors.MessageConnector
import javax.inject.Inject
import models.messages.ArrivalNotification
import models.messages.request.{ArrivalNotificationRequest, MessageSender, RequestModelError}
import models.{ArrivalNotificationXSD, Source, WebChannel}
import play.api.libs.json.{JsError, Reads}
import play.api.mvc._
import repositories.ArrivalNotificationRepository
import services._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class ArrivalNotificationController @Inject()(
                                               cc: ControllerComponents,
                                               xmlSubmissionService: XmlSubmissionService,
                                               bodyParsers: PlayBodyParsers,
                                               interchangeControlReferenceService: InterchangeControlReferenceService,
                                               arrivalNotificationRepository: ArrivalNotificationRepository,
                                               messageConnector: MessageConnector,
                                               appConfig: AppConfig,
                                               submissionModelService: SubmissionModelService,
                                               xmlBuilderService: XmlBuilderService,
                                               xmlValidationService: XmlValidationService
                                             )
  extends BackendController(cc) {

  def validateJson[A: Reads]: BodyParser[A] = bodyParsers.json.validate(
    _.validate[A].asEither.left.map(e =>
      BadRequest(JsError.toJson(e)).as("application/json")
    ))

  def post(): Action[ArrivalNotification] = Action.async(validateJson[ArrivalNotification]) {
    implicit request =>

      val messageSender = MessageSender(appConfig.env, "eori")
      val arrivalNotification = request.body

      implicit val localDateTime: LocalDateTime = LocalDateTime.now()

      interchangeControlReferenceService.getInterchangeControlReferenceId.flatMap {
        case Right(interchangeControlReferenceId) => {
          submissionModelService.convertFromArrivalNotification(arrivalNotification, messageSender, interchangeControlReferenceId) match {
            case Right(arrivalNotificationRequest) => {
              xmlBuilderService.buildXml(arrivalNotificationRequest) match {
                case Right(node) => {
                  xmlValidationService.validate(node.toString(), ArrivalNotificationXSD) match {
                    case Right(_) => {
                      saveAndSubmit(arrivalNotification, arrivalNotificationRequest, node.toString(), WebChannel)
                    }
                    case Left(_: RequestModelError) => Future.successful(InternalServerError)
                  }
                }
                case Left(_: RequestModelError) => {
                  Future.successful(InternalServerError)
                }
              }
            }
            case Left(_: RequestModelError) => {
              Future.successful(BadRequest)
            }
          }
        }
        case Left(FailedCreatingInterchangeControlReference) => {
          Future.successful(InternalServerError)
        }
        case _ => {
          Future.successful(InternalServerError)
        }
      }
  }

  private def saveAndSubmit(
                             arrivalNotification: ArrivalNotification,
                             arrivalNotificationRequest: ArrivalNotificationRequest,
                             xml: String,
                             channel: Source
                           )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {

    arrivalNotificationRepository.persistToMongo(arrivalNotification).flatMap {
      _ =>
        messageConnector.post(xml.toString, arrivalNotificationRequest.messageCode, WebChannel)
    }
  }

}