/*
 * Copyright 2023 HM Revenue & Customs
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

import audit.AuditService
import base.SpecBase
import generators.ModelGenerators
import models.Arrival
import models.ArrivalId
import models.ArrivalNotFoundError
import models.ArrivalWithoutMessages
import models.MessageId
import models.MessageType
import models.MovementMessageWithoutStatus
import models.UnloadingPermissionResponse
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.ArrivalMovementRepository

import java.time.LocalDateTime
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

    "getArrivalAndAudit" - {

      val messageResponse = UnloadingPermissionResponse
      val requestXml      = <xml>test</xml>
      val movementMessage =
        MovementMessageWithoutStatus(MessageId(1), LocalDateTime.now, Some(LocalDateTime.now), MessageType.UnloadingPermission, requestXml, 1)
      val mockAuditService = mock[AuditService]

      "must return an Arrival and not audit" in {
        forAll(arbitrary[ArrivalWithoutMessages]) {
          arrival =>
            when(mockArrivalMovementRepository.getWithoutMessages(any())).thenReturn(Future.successful(Some(arrival)))

            val application = baseApplicationBuilder
              .overrides(
                bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
                bind[AuditService].toInstance(mockAuditService)
              )
              .build()

            running(application) {
              val service = application.injector.instanceOf[GetArrivalService]

              val result = service.getArrivalAndAudit(ArrivalId(0), messageResponse, movementMessage).value.futureValue

              result mustBe Right(arrival)
              verify(mockAuditService, never()).auditNCTSRequestedMissingMovementEvent(any(), any(), any())(any())
              reset(mockAuditService)
            }
        }
      }

      "must return ArrivalNotFoundError and audit when arrival is missing" in {

        when(mockArrivalMovementRepository.getWithoutMessages(any())).thenReturn(Future.successful(None))

        val application = baseApplicationBuilder
          .overrides(
            bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val service   = application.injector.instanceOf[GetArrivalService]
          val arrivalId = ArrivalId(0)
          val result    = service.getArrivalAndAudit(ArrivalId(0), messageResponse, movementMessage).value.futureValue

          result.left.value mustBe an[ArrivalNotFoundError]
          verify(mockAuditService, times(1)).auditNCTSRequestedMissingMovementEvent(eqTo(arrivalId), eqTo(messageResponse), eqTo(movementMessage))(any())
          reset(mockAuditService)
        }
      }

    }
  }
}
