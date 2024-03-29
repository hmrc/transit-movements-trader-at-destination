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

package models

abstract class XSDFile(val filePath: String)

object XSDFile {
  object GoodsReleasedXSD            extends XSDFile("/xsd/CC025A.xsd")
  object ArrivalRejectedXSD          extends XSDFile("/xsd/CC008A.xsd")
  object UnloadingPermissionXSD      extends XSDFile("/xsd/CC043A.xsd")
  object UnloadingRemarksRejectedXSD extends XSDFile("/xsd/CC058A.xsd")
  object InvalidXmlXSD               extends XSDFile("/xsd/CC917A.xsd")
}
