/*
 * Copyright 2022 HM Revenue & Customs
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

import base.SpecBase
import generators.ModelGenerators
import models.MessageType._

class MessageResponseSpec extends SpecBase with ModelGenerators {

  "MessageResponseSpec" - {

    "getMessageResponseFromCode" - {

      "must return correct MessageResponse" - {

        "for GoodsReleased code" in {

          val result = MessageResponse.getMessageResponseFromCode(GoodsReleased.rootNode)

          result.value mustBe GoodsReleasedResponse
        }

        "for ArrivalRejection code" in {

          val result = MessageResponse.getMessageResponseFromCode(ArrivalRejection.rootNode)

          result.value mustBe ArrivalRejectedResponse
        }

        "for UnloadingPermission code" in {

          val result = MessageResponse.getMessageResponseFromCode(UnloadingPermission.rootNode)

          result.value mustBe UnloadingPermissionResponse
        }

        "for UnloadingRemarks code" in {

          val result = MessageResponse.getMessageResponseFromCode(UnloadingRemarks.rootNode)

          result.value mustBe UnloadingRemarksResponse
        }

        "for UnloadingRemarksRejection code" in {

          val result = MessageResponse.getMessageResponseFromCode(UnloadingRemarksRejection.rootNode)

          result.value mustBe UnloadingRemarksRejectedResponse
        }

        "for XMLSubmissionNegativeAcknowledgement code" in {

          val result = MessageResponse.getMessageResponseFromCode(XMLSubmissionNegativeAcknowledgement.rootNode)

          result.value mustBe XMLSubmissionNegativeAcknowledgementResponse
        }
      }

      "must return InvalidArrivalRootNode for an unrecognised code" in {

        val result = MessageResponse.getMessageResponseFromCode("InvalidCode")

        result.left.value mustBe an[InvalidArrivalRootNodeError]
      }
    }
  }
}
