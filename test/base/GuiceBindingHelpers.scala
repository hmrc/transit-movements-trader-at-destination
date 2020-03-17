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

package base

import controllers.actions.FakeIdentifierAction
import controllers.actions.IdentifierAction
import org.scalatest.TestSuite
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder

import scala.reflect.ClassTag

trait GuiceBindingHelpers extends GuiceOneAppPerSuite {
  self: TestSuite =>

  protected def applicationBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .overrides(
        bind[IdentifierAction].to[FakeIdentifierAction]
      )

  /*
  The functions below are an attempt to refactor the massive and complex chaining of
   */

  protected val startWithApplicationBuilder: Unit => GuiceApplicationBuilder = _ => new GuiceApplicationBuilder()

  protected def withFakeIdentifierAction(applicationBuilder: GuiceApplicationBuilder): GuiceApplicationBuilder =
    applicationBuilder.overrides(bind[IdentifierAction].to[FakeIdentifierAction])

  protected def withBinding[A: ClassTag](a: A): GuiceApplicationBuilder => GuiceApplicationBuilder =
    _.overrides(bind[A].toInstance(a))

}
