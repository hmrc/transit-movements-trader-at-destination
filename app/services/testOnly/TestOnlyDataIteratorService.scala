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

package services.testOnly

import models.testOnly.SeedDataParameters
import models.testOnly.SeedEori
import models.testOnly.SeedMrn

object TestOnlyDataIteratorService {

  def seedDataIterator(seedDataParameters: SeedDataParameters): Iterator[(SeedEori, SeedMrn)] = {
    val SeedDataParameters(
      startEori,
      numberOfUsers,
      startMrn,
      movementsPerUser
    ) = seedDataParameters

    Iterator
      .from(startEori.suffix.toInt, numberOfUsers) // TODO: Deal with long
      .map(SeedEori(startEori.prefix, _, startEori.padLength))
      .flatMap {
        seedEori =>
          Iterator
            .from(startMrn.suffix.toInt, movementsPerUser) // TODO: Deal with long
            .map(SeedMrn(startMrn.prefix, _, startMrn.padLength))
            .map(x => (seedEori, x))
      }
  }

}
