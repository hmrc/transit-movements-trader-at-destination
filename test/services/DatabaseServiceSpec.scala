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

package services

import base.SpecBase
import generators.MessageGenerators
import models.ArrivalMovement
import models.request.InternalReferenceId
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories._

import scala.concurrent.Future

class DatabaseServiceSpec
    extends FreeSpec
    with MustMatchers
    with MockitoSugar
    with ScalaFutures
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with SpecBase
    with BeforeAndAfterEach {

  private val mockInternalReferenceRepository = mock[InternalReferenceIdRepository]
  private val mockArrivalMovementRepository   = mock[ArrivalMovementRepository]

  val service = new DatabaseServiceImpl(
    mockInternalReferenceRepository,
    mockArrivalMovementRepository
  )

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockInternalReferenceRepository)
    reset(mockArrivalMovementRepository)
  }

  "DatabaseService" - {

    "getInternalReferenceId" - {

      "must return a movement reference id when successful" in {

        val internalReferenceId = InternalReferenceId(0)

        when(mockInternalReferenceRepository.nextId())
          .thenReturn(Future.successful(internalReferenceId))

        val response = service.getInternalReferenceId.futureValue

        response mustBe Right(internalReferenceId)
      }

      "must return FailedCreatingMovementReference when failed" in {

        when(mockInternalReferenceRepository.nextId())
          .thenReturn(Future.failed(new RuntimeException))

        val response = service.getInternalReferenceId.futureValue

        response mustBe Left(FailedCreatingNextInternalReferenceId)
      }

    }

    "saveArrivalNotification" - {

      "must return WriteResult when successful" in {

        forAll(arbitrary[ArrivalMovement]) {
          arrivalNotification =>
            when(mockArrivalMovementRepository.persistToMongo(arrivalNotification))
              .thenReturn(Future.successful(fakeWriteResult))

            val response = service.saveArrivalMovement(arrivalNotification).futureValue

            response mustBe Right(fakeWriteResult)
        }

      }

      "must return FailedSavingArrivalNotification when failed" in {

        forAll(arbitrary[ArrivalMovement]) {
          arrivalNotification =>
            when(mockArrivalMovementRepository.persistToMongo(arrivalNotification))
              .thenReturn(Future.failed(new RuntimeException))

            val response = service.saveArrivalMovement(arrivalNotification).futureValue

            response mustBe Left(FailedSavingArrivalNotification)
        }
      }
    }
  }

}
