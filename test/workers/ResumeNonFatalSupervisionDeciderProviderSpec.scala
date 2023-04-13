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

package workers

import akka.stream.Supervision
import base.SpecBase
import play.api.Logger

import scala.util.control.ControlThrowable

class ResumeNonFatalSupervisionDeciderProviderSpec extends SpecBase {
  val mockLogger  = mock[Logger]
  private val sut = ResumeNonFatalSupervisionDeciderProvider("worker-name", mockLogger)

  "when a fatal exception is received then a Stop Directive is returned" in {

    sut.supervisionDecider(new ControlThrowable {}) must equal(Supervision.Stop)

  }

  "when a non-fatal exception is received then a Resume Directive is returned" in {

    sut.supervisionDecider(new RuntimeException("a non-fatal exception")) must equal(Supervision.Resume)

  }

}
