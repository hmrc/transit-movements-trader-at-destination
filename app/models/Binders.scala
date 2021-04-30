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

package models

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import play.api.mvc.QueryStringBindable

object Binders {
  implicit val offsetDateTimeQueryStringBindable: QueryStringBindable[OffsetDateTime] = {
    new QueryStringBindable.Parsing[OffsetDateTime](
      OffsetDateTime.parse(_),
      dt => DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(dt),
      (param, exc) => s"Cannot parse parameter $param as OffsetDateTime: ${exc.getMessage}"
    )
  }
}
