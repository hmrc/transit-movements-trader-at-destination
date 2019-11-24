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

package services

import models.messages.request.InterchangeControlReference
import org.scalatest.{FreeSpec, MustMatchers}
import repositories.{ArrivalNotificationRepository, SequentialInterchangeControlReferenceIdRepository}
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class DatabaseServiceSpec extends
  FreeSpec with MustMatchers with MockitoSugar with ScalaFutures {

  val mockRepository = mock[SequentialInterchangeControlReferenceIdRepository]
  val mockArrivalNotificationRepository = mock[ArrivalNotificationRepository]

  "DatabaseService" - {

    "must return InterchangeControlReference when successful" in {

      val service = new DatabaseServiceImpl(mockRepository, mockArrivalNotificationRepository)

      when(mockRepository.nextInterchangeControlReferenceId())
          .thenReturn(Future.successful(InterchangeControlReference("date", 1)))

      val response = service.getInterchangeControlReferenceId.futureValue

      response mustBe Right(InterchangeControlReference("date", 1))
    }

    "must return FailedCreatingInterchangeControlReference when failed" in {

      val service = new DatabaseServiceImpl(mockRepository, mockArrivalNotificationRepository)

      when(mockRepository.nextInterchangeControlReferenceId())
        .thenReturn(Future.failed(new RuntimeException))

      val response = service.getInterchangeControlReferenceId.futureValue

      response mustBe Left(FailedCreatingInterchangeControlReference)
    }

  }

}
