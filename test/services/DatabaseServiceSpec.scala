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
import models.request.InterchangeControlReference
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories.ArrivalMovementRepository
import repositories.ArrivalNotificationRepository
import repositories.FailedSavingArrivalNotification
import repositories.SequentialInterchangeControlReferenceIdRepository

import scala.concurrent.Future

class DatabaseServiceSpec
    extends FreeSpec
    with MustMatchers
    with MockitoSugar
    with ScalaFutures
    with ScalaCheckPropertyChecks
    with MessageGenerators
    with SpecBase {

  val mockRepository                = mock[SequentialInterchangeControlReferenceIdRepository]
  val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

  "DatabaseService" - {

    "getInterchangeControlReferenceId" - {

      "must return InterchangeControlReference when successful" in {

        val service = new DatabaseServiceImpl(mockRepository, mockArrivalMovementRepository)

        when(mockRepository.nextInterchangeControlReferenceId())
          .thenReturn(Future.successful(InterchangeControlReference("date", 1)))

        val response = service.getInterchangeControlReferenceId.futureValue

        response mustBe Right(InterchangeControlReference("date", 1))
      }

      "must return FailedCreatingInterchangeControlReference when failed" in {

        val service = new DatabaseServiceImpl(mockRepository, mockArrivalMovementRepository)

        when(mockRepository.nextInterchangeControlReferenceId())
          .thenReturn(Future.failed(new RuntimeException))

        val response = service.getInterchangeControlReferenceId.futureValue

        response mustBe Left(FailedCreatingInterchangeControlReference)
      }

    }

    "saveArrivalNotification" - {

      "must return WriteResult when successful" in {

        forAll(arbitrary[ArrivalMovement]) {
          arrivalNotification =>
            val service = new DatabaseServiceImpl(mockRepository, mockArrivalMovementRepository)

            when(mockArrivalMovementRepository.persistToMongo(arrivalNotification))
              .thenReturn(Future.successful(fakeWriteResult))

            val response = service.saveArrivalNotification(arrivalNotification).futureValue

            response mustBe Right(fakeWriteResult)
        }

      }

      "must return FailedSavingArrivalNotification when failed" in {

        forAll(arbitrary[ArrivalMovement]) {
          arrivalNotification =>
            val service = new DatabaseServiceImpl(mockRepository, mockArrivalMovementRepository)

            when(mockArrivalMovementRepository.persistToMongo(arrivalNotification))
              .thenReturn(Future.failed(new RuntimeException))

            val response = service.saveArrivalNotification(arrivalNotification).futureValue

            response mustBe Left(FailedSavingArrivalNotification)
        }

      }

    }

  }

}
