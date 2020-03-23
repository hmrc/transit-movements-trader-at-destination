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

package controllers.actions

import generators.MessageGenerators
import models.Arrival
import models.request.ArrivalRequest
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.OptionValues
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Action
import play.api.mvc.ActionBuilder
import play.api.mvc.ActionFunction
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.Request
import play.api.mvc.Results
import repositories.ArrivalMovementRepository
import org.scalacheck.Arbitrary.arbitrary
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future

class ArrivalRetrievalActionSpec extends FreeSpec with MustMatchers with MockitoSugar with ScalaCheckPropertyChecks with MessageGenerators with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(retrievalAction: ActionBuilder[ArrivalRequest, AnyContent] with ActionFunction[Request, ArrivalRequest]) {

    def onPageLoad(): Action[AnyContent] = retrievalAction {
      request =>
        Results.Ok(request.arrival.arrivalId.toString)
    }
  }

  "movement retrieval action" - {

    "must retrieve a movement when one exists" in {

      val arrival = arbitrary[Arrival].sample.value

      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(Some(arrival)))

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[IdentifierAction].to[FakeIdentifierAction],
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )

      val actionProvider = application.injector.instanceOf[ArrivalRetrievalActionProvider]

      val action     = actionProvider(arrival.arrivalId)
      val controller = new Harness(action)
      val result     = controller.onPageLoad()(fakeRequest)

      status(result) mustEqual OK
      contentAsString(result) mustEqual arrival.arrivalId.toString
    }

    "must return NotFound when one does not exist" in {

      val arrival = arbitrary[Arrival].sample.value

      val mockArrivalMovementRepository = mock[ArrivalMovementRepository]

      when(mockArrivalMovementRepository.get(any())).thenReturn(Future.successful(None))

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[IdentifierAction].to[FakeIdentifierAction],
          bind[ArrivalMovementRepository].toInstance(mockArrivalMovementRepository)
        )

      val actionProvider = application.injector.instanceOf[ArrivalRetrievalActionProvider]

      val action     = actionProvider(arrival.arrivalId)
      val controller = new Harness(action)
      val result     = controller.onPageLoad()(fakeRequest)

      status(result) mustEqual NOT_FOUND
    }
  }
}
