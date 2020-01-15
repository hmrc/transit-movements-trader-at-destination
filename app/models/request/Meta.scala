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

package models.request

case class Meta(messageSender: MessageSender,
                interchangeControlReference: InterchangeControlReference,
                senderIdentificationCodeQualifier: Option[String] = None,
                recipientIdentificationCodeQualifier: Option[String] = None,
                recipientsReferencePassword: Option[String] = None,
                recipientsReferencePasswordQualifier: Option[String] = None,
                priority: Option[String] = None,
                acknowledgementRequest: Option[String] = None,
                communicationsAgreementId: Option[String] = None,
                commonAccessReference: Option[String] = None,
                messageSequenceNumber: Option[String] = None,
                firstAndLastTransfer: Option[String] = None)
    extends MetaConstants

sealed trait MetaConstants {
  val syntaxIdentifier: String     = "UNOC"
  val syntaxVersionNumber: String  = "3"
  val messageRecipient: String     = "NCTS"
  val applicationReference: String = "NCTS"
  val messageIndication            = "1"
  val testIndicator                = "0"
}
