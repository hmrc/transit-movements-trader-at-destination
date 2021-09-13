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

package services

import base.SpecBase
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository

import scala.concurrent.Future

class GetArrivalServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

  "GetArrivalService" - {
    "GetArrivalById" - {

      "must return an Arrival" in {
        forAll(arbitrary[Arrival]) {
          arrival =>
            when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

            val application = baseApplicationBuilder.overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)).build()

            running(application) {
              val service = application.injector.instanceOf[GetArrivalService]

              val result = service.getArrivalById(ArrivalId(0)).futureValue.value

              result mustBe arrival
            }
        }
      }

      "must return ArrivalNotFoundError when arrival is missing" in {

        when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder.overrides(bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)).build()

        running(application) {
          val service = application.injector.instanceOf[GetArrivalService]

          val result = service.getArrivalById(ArrivalId(0)).futureValue.left.value

          result mustBe an[ArrivalNotFoundError]
        }
      }
    }
  }
}
