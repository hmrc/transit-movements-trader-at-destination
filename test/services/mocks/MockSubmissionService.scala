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

package services.mocks

import models.messages.ArrivalNotification
import org.scalatest.{BeforeAndAfterEach, Suite}
import org.scalatestplus.mockito.MockitoSugar
import services.SubmissionService
import org.mockito.Mockito._

trait MockSubmissionService extends MockitoSugar with BeforeAndAfterEach {
  this: Suite =>

  val mockSubmissionService = mock[SubmissionService]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockSubmissionService)
  }

  def mockSubmit(response: Int, arrivalNotification: ArrivalNotification): Unit = {
    when(mockSubmissionService.submit(arrivalNotification))
      .thenReturn(response)
  }
}
