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

import java.time.LocalDateTime

import cats.data.NonEmptyList
import javax.inject.Inject
import models.ArrivalStatus.Initialized
import models.MessageType.ArrivalNotification
import models.Arrival
import models.ChannelType
import models.MessageStatus
import models.MovementMessageWithStatus
import models.MovementReferenceNumber
import play.api.i18n.MessagesApi
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import repositories.ArrivalIdRepository
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class TestOnlySeedDataController @Inject()(override val messagesApi: MessagesApi, arrivalIdRepository: ArrivalIdRepository, cc: ControllerComponents)(
  implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def seedData: Action[SeedDataParameters] = Action(parse.json[SeedDataParameters]) {
    implicit request =>
      Ok(Json.toJson(output(request.body)))
  }

  private def output(seedDataParameters: SeedDataParameters): SeedDataResponse = {

    val SeedDataParameters(
      startEori,
      numberOfUsers,
      startMrn,
      movementsPerUser
    ) = seedDataParameters

    val eoriRange: immutable.Seq[Long] = startEori.suffix to (startEori.suffix + numberOfUsers)

    val rangeOfEori: Seq[SeedEori] = eoriRange.map {
      suffix =>
        SeedEori(startEori.prefix, suffix, startEori.padLength)
    }

    val mrnRange: immutable.Seq[Long] = startMrn.suffix to (startMrn.suffix + movementsPerUser)

    val rangeOfMrn: Seq[SeedMrn] = mrnRange.map {
      suffix =>
        SeedMrn(startMrn.prefix, suffix, startMrn.padLength)
    }


    val arrivalList: Future[immutable.Seq[Arrival]] = Future.sequence {
      eoriRange.flatMap { suffix =>
        val eori = SeedEori(startEori.prefix, suffix, startEori.padLength).format

        mrnRange.map { suffix =>
          val mrn = SeedMrn(startMrn.prefix, suffix, startMrn.padLength).format
          makeArrivalMovement(eori, mrn)
        }
      }
    }

    SeedDataResponse(startEori, rangeOfEori.last, movementsPerUser, startMrn, rangeOfMrn.last)
  }

  def makeArrivalMovement(eori: String, mrn: String): Future[Arrival] = {

    val dateTime = LocalDateTime.now()

    val movementMessage = MovementMessageWithStatus(
      dateTime,
      ArrivalNotification,
      NodeSeq.Empty,
      MessageStatus.SubmissionPending,
      1,
      JsObject.empty
    )

    arrivalIdRepository.nextId().map {
      arrivalId =>
        Arrival(
          arrivalId,
          ChannelType.web,
          MovementReferenceNumber(mrn),
          eori,
          Initialized,
          dateTime,
          dateTime,
          dateTime,
          NonEmptyList.one(movementMessage),
          2
        )
    }
  }

}
