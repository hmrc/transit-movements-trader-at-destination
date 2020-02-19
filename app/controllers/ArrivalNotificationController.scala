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

package controllers

import java.time.LocalDateTime
import java.time.OffsetDateTime

import config.AppConfig
import connectors.MessageConnector
import helpers.XmlBuilderHelper
import javax.inject.Inject
import models.messages.ArrivalNotificationMessage
import models.request._
import play.api.libs.json.JsError
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.mvc._
import reactivemongo.api.commands.WriteResult
import repositories.FailedSavingArrivalNotification
import services._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.BackendController
import utils.ErrorResponseBuilder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Node

class ArrivalNotificationController @Inject()(
  cc: ControllerComponents,
  bodyParsers: PlayBodyParsers,
  appConfig: AppConfig,
  databaseService: DatabaseService,
  messageConnector: MessageConnector,
  submissionModelService: SubmissionModelService,
  xmlBuilderService: XmlBuilderHelper,
  xmlValidationService: XmlValidationService
) extends BackendController(cc) {

  def post(): Action[ArrivalNotificationMessage] = Action.async(validateJson[ArrivalNotificationMessage]) {
    implicit request =>
      val messageSender = MessageSender(appConfig.env, "eori")

      val arrivalNotification = request.body

      val localDateTime: LocalDateTime = LocalDateTime.now()

      databaseService.getInterchangeControlReferenceId.flatMap {

        case Right(interchangeControlReferenceId) => {

          submissionModelService.convertToSubmissionModel(arrivalNotification, messageSender, interchangeControlReferenceId) match {

            case Right(arrivalNotificationRequestModel) => {

              val arrivalNotificationRequestXml = arrivalNotificationRequestModel.toXml(localDateTime)

              xmlValidationService.validate(arrivalNotificationRequestXml.toString(), ArrivalNotificationXSD) match {

                case Right(XmlSuccessfullyValidated) => {

                  val xmlWithWrapper: Node = arrivalNotificationRequestModel.addTransitWrapper(arrivalNotificationRequestXml)

                  databaseService
                    .saveArrivalNotification(arrivalNotification)
                    .flatMap {
                      sendMessage(xmlWithWrapper, arrivalNotificationRequestModel)
                    }
                    .recover {
                      case _ =>
                        InternalServerError(Json.toJson(ErrorResponseBuilder.failedSavingArrivalNotification))
                          .as("application/json")
                    }
                }
                case Left(FailedToValidateXml(reason)) =>
                  Future.successful(
                    BadRequest(Json.toJson(ErrorResponseBuilder.failedXmlValidation(reason)))
                      .as("application/json"))
              }
            }
            case Left(FailedToConvertModel) =>
              Future.successful(
                BadRequest(Json.toJson(ErrorResponseBuilder.failedToCreateRequestModel))
                  .as("application/json"))
          }
        }
        case Left(FailedCreatingInterchangeControlReference) =>
          Future.successful(
            InternalServerError(Json.toJson(ErrorResponseBuilder.failedToCreateInterchangeControlRef))
              .as("application/json")
          )
      }
  }

  private def sendMessage(xml: Node, arrivalNotificationRequestModel: ArrivalNotificationRequest)(
    implicit headerCarrier: HeaderCarrier): PartialFunction[Either[FailedSavingArrivalNotification, WriteResult], Future[Result]] = {
    case Right(_) => {
      messageConnector
        .post(xml.toString, arrivalNotificationRequestModel.xMessageType, OffsetDateTime.now)
        .map {
          _ =>
            NoContent
        }
        .recover {
          case _ =>
            BadGateway(Json.toJson(ErrorResponseBuilder.failedSubmissionToEIS))
              .as("application/json")
        }
    }
    case Left(FailedSavingArrivalNotification) =>
      Future.successful(
        InternalServerError(Json.toJson(ErrorResponseBuilder.failedSavingToDatabase))
          .as("application/json"))
  }

  private def validateJson[A: Reads]: BodyParser[A] =
    bodyParsers.json.validate(_.validate[A].asEither.left.map(e => BadRequest(JsError.toJson(e)).as("application/json")))

}
