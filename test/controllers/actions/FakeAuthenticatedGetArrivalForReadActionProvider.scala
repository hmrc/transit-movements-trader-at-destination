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

import models.Arrival
import models.ArrivalId
import models.request.ArrivalRequest
import org.scalatest.exceptions.TestFailedException
import play.api.mvc._
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class FakeAuthenticatedGetArrivalForReadActionProvider(arrival: Arrival) extends AuthenticatedGetArrivalForReadActionProvider {
  override def apply(arrivalId: ArrivalId): ActionBuilder[ArrivalRequest, AnyContent] =
    new ActionBuilder[ArrivalRequest, AnyContent] {
      override def parser: BodyParser[AnyContent] = stubBodyParser()

      override def invokeBlock[A](request: Request[A], block: ArrivalRequest[A] => Future[Result]): Future[Result] =
        if (arrival.arrivalId == arrivalId) {
          block(ArrivalRequest(request, arrival))
        } else {
          throw new TestFailedException(
            s"Bad test data setup. ArrivalId on the Arrival was ${arrival.arrivalId} but expected to retrieve arrival with Id of $arrivalId",
            0
          )
        }

      override protected def executionContext: ExecutionContext = implicitly[ExecutionContext]

    }
}

object FakeAuthenticatedGetArrivalForReadActionProvider {

  def apply(arrival: Arrival): FakeAuthenticatedGetArrivalForReadActionProvider =
    new FakeAuthenticatedGetArrivalForReadActionProvider(arrival)
}
