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

package config

import com.google.inject.AbstractModule
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticateActionProviderImpl
import controllers.actions.AuthenticatedGetArrivalForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalForReadActionProviderImpl
import controllers.actions.AuthenticatedGetArrivalWithoutMessagesForReadActionProvider
import controllers.actions.AuthenticatedGetArrivalWithoutMessagesForReadActionProviderImpl
import controllers.actions.AuthenticatedGetMessagesForReadActionProvider
import controllers.actions.AuthenticatedGetMessagesForReadActionProviderImpl
import repositories.ArrivalMovementRepository
import utils.MessageTranslation
import java.time.Clock

import migrations.MigrationRunner
import migrations.MigrationRunnerImpl

class Module extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[AppConfig]).asEagerSingleton()
    bind(classOf[ArrivalMovementRepository]).asEagerSingleton()
    bind(classOf[AuthenticateActionProvider]).to(classOf[AuthenticateActionProviderImpl]).asEagerSingleton()
    bind(classOf[AuthenticatedGetArrivalForReadActionProvider]).to(classOf[AuthenticatedGetArrivalForReadActionProviderImpl])
    bind(classOf[AuthenticatedGetArrivalWithoutMessagesForReadActionProvider]).to(classOf[AuthenticatedGetArrivalWithoutMessagesForReadActionProviderImpl])
    bind(classOf[AuthenticatedGetMessagesForReadActionProvider]).to(classOf[AuthenticatedGetMessagesForReadActionProviderImpl]).asEagerSingleton()
    bind(classOf[MessageTranslation]).asEagerSingleton()
    bind(classOf[StreamLoggingConfig]).to(classOf[StreamLoggingConfigImpl]).asEagerSingleton()
    bind(classOf[Clock]).toInstance(Clock.systemUTC)
    bind(classOf[MigrationRunner]).to(classOf[MigrationRunnerImpl]).asEagerSingleton()
  }
}
