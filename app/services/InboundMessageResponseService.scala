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

package services

import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture
import models.CannotFindRootNodeError
import models.FailedToValidateMessage
import models.InboundMessageResponse
import models.MessageResponse
import models.SubmissionState

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class InboundMessageResponseService @Inject()(xmlValidationService: XmlValidationService)(implicit ec: ExecutionContext) {

  def makeInboundMessageResponse(xml: NodeSeq): EitherT[Future, SubmissionState, InboundMessageResponse] =
    for {
      headNode                <- EitherT.fromOption(xml.headOption, CannotFindRootNodeError(s"[InboundRequest][inboundRequest] Could not find root node"))
      messageResponse         <- EitherT.fromEither(MessageResponse.getMessageResponseFromCode(headNode.label))
      validateInboundResponse <- EitherT.fromEither(MessageValidationService.validateInboundMessage(messageResponse))
      _ <- EitherT.fromEither(
        xmlValidationService
          .validate(xml.toString, validateInboundResponse.xsdFile)
          .toOption
          .toRight[SubmissionState](FailedToValidateMessage(s"[InboundRequest][makeInboundMessageResponse] XML failed to validate against XSD file")))
    } yield validateInboundResponse
}
