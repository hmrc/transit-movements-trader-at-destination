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

import java.time.LocalDate
import java.time.LocalTime

import base.SpecBase
import generators.MessageGenerators
import models.ArrivalMovement
import models.request.InternalReferenceId
import org.mockito.Matchers.any
import org.mockito.Mockito.reset
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.ArrivalMovementRepository
import repositories.FailedCreatingNextInternalReferenceId
import services.DatabaseService
import utils.Format

import scala.concurrent.Future

class MovementsControllerSpec extends SpecBase with ScalaCheckPropertyChecks with MessageGenerators with BeforeAndAfterEach with IntegrationPatience {

  private val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

  def applicationMovementsControllerSpec =
    applicationBuilder
      .overrides(
        bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
      )

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockArrivalMovementRepository)
  }

  "MovementsController" - {

    "createMovement" - {

      "must return Ok and create movement" in {
        val mockDatabaseService = mock[DatabaseService]
        val ir                  = InternalReferenceId(1)
        when(mockDatabaseService.getInternalReferenceId).thenReturn(Future.successful(Right(ir)))
        when(mockDatabaseService.saveArrivalMovement(any())).thenReturn(Future.successful(Right(fakeWriteResult)))

        val application = applicationMovementsControllerSpec.overrides(bind[DatabaseService].toInstance(mockDatabaseService)).build()

        running(application) {

          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <transitRequest>
              <CC007A>
                <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
                <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
                <HEAHEA>
                  <DocNumHEA5>MRN</DocNumHEA5>
                </HEAHEA>
              </CC007A>
            </transitRequest>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(ir.index.toString) // TODO: This needs to be the actual resource location
          verify(mockDatabaseService, times(1)).saveArrivalMovement(any())

        }
      }

      "must return InternalServerError if the InternalReference generation fails" in {
        val mockDatabaseService = mock[DatabaseService]
        when(mockDatabaseService.getInternalReferenceId).thenReturn(Future.successful(Left(FailedCreatingNextInternalReferenceId)))

        val application =
          applicationMovementsControllerSpec
            .overrides(bind[DatabaseService].toInstance(mockDatabaseService))
            .build()

        running(application) {
          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <transitRequest>
              <CC007A>
                <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
                <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
                <HEAHEA>
                  <DocNumHEA5>MRN</DocNumHEA5>
                </HEAHEA>
              </CC007A>
            </transitRequest>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be (defined)
        }
      }

      "must return InternalServerError if the database fails to create a new Arrival Movement" in {
        val mockDatabaseService = mock[DatabaseService]
        when(mockDatabaseService.getInternalReferenceId).thenReturn(Future.successful(Left(FailedCreatingNextInternalReferenceId)))

        val application =
          applicationMovementsControllerSpec
            .overrides(bind[DatabaseService].toInstance(mockDatabaseService))
            .build()

        running(application) {
          val dateOfPrep = LocalDate.now()
          val timeOfPrep = LocalTime.of(1, 1)

          val requestXmlBody =
            <transitRequest>
              <CC007A>
                <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
                <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
                <HEAHEA>
                  <DocNumHEA5>MRN</DocNumHEA5>
                </HEAHEA>
              </CC007A>
            </transitRequest>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be (defined)
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockDatabaseService = mock[DatabaseService]
        when(mockDatabaseService.getInternalReferenceId).thenReturn(Future.successful(Left(FailedCreatingNextInternalReferenceId)))

        val application =
          applicationMovementsControllerSpec
            .overrides(bind[DatabaseService].toInstance(mockDatabaseService))
            .build()

        running(application) {
          val requestXmlBody =
            <transitRequest>
              <CC007A>
                <HEAHEA>
                </HEAHEA>
              </CC007A>
            </transitRequest>

          val request = FakeRequest(POST, routes.MovementsController.createMovement().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be (defined)
        }
      }
    }

    "getMovements" - {

      "must return Ok and retrieve movements" in {
        forAll(seqWithMaxLength[ArrivalMovement](10)) {
          arrivalMovements =>
            val application = applicationMovementsControllerSpec.build()

            running(application) {
              when(mockArrivalMovementRepository.fetchAllMovements(any())).thenReturn(Future.successful(arrivalMovements))

              val expectedResult = arrivalMovements.map(_.messages.head)

              val request = FakeRequest(GET, routes.MovementsController.getMovements().url)

              val result = route(application, request).value

              status(result) mustEqual OK
              contentAsJson(result) mustEqual Json.toJson(expectedResult)
            }
        }
      }

      "must return an INTERNAL_SERVER_ERROR on fail" in {
        val application = applicationMovementsControllerSpec.build()

        running(application) {
          when(mockArrivalMovementRepository.fetchAllMovements(any()))
            .thenReturn(Future.failed(new Exception))

          val request = FakeRequest(GET, routes.MovementsController.getMovements().url)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }

  }
}
