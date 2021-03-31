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

import java.time.Clock
import java.time.LocalDateTime
import cats.data.NonEmptyList

import javax.inject.Inject
import models.ArrivalStatus.Initialized
import models.MessageType.ArrivalNotification
import models.Arrival
import models.ArrivalId
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

class TestOnlySeedDataController @Inject()(override val messagesApi: MessagesApi, cc: ControllerComponents, clock: Clock)(implicit ec: ExecutionContext)
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

    val dataToInsert: Iterator[Arrival] = seedArrivals(seedDataParameters) // TODO: Use this

    val maxEori: SeedEori = SeedEori(startEori.prefix, startEori.suffix + numberOfUsers, startEori.padLength)
    val maxMrn: SeedMrn   = SeedMrn(startMrn.prefix, startMrn.suffix + movementsPerUser, startMrn.padLength)
    SeedDataResponse(startEori, maxEori, movementsPerUser, startMrn, maxMrn)
  }

  private def seedDataIterator(seedDataParameters: SeedDataParameters): Iterator[(SeedEori, SeedMrn)] = {
    val SeedDataParameters(
      startEori,
      numberOfUsers,
      startMrn,
      movementsPerUser
    ) = seedDataParameters

    Iterator
      .from(startEori.suffix.toInt, numberOfUsers) // TODO: Deal with long
      .map(SeedEori(startEori.prefix, _, startEori.padLength))
      .flatMap {
        seedEori =>
          Iterator
            .from(startMrn.suffix.toInt, movementsPerUser) // TODO: Deal with long
            .map(SeedMrn(startMrn.prefix, _, startMrn.padLength))
            .map(x => (seedEori, x))
      }
  }

  private def makeArrivalMovement(eori: String, mrn: String, arrivalId: ArrivalId): Arrival = {

    val dateTime = LocalDateTime.now(clock)

    val movementMessage = MovementMessageWithStatus(
      dateTime,
      ArrivalNotification,
      NodeSeq.Empty,
      MessageStatus.SubmissionPending,
      1,
      JsObject.empty
    )

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

  private def seedArrivals(seedDataParameters: SeedDataParameters): Iterator[Arrival] =
    for {
      arrivalId   <- Iterator.from(999999).map(ArrivalId(_))
      (eori, mrn) <- seedDataIterator(seedDataParameters)
    } yield makeArrivalMovement(eori.format, mrn.format, arrivalId)

}
