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

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import helpers.XmlBuilderHelper
import utils.Format

import scala.xml.NodeSeq

case class Meta(messageSender: MessageSender,
                interchangeControlReference: InterchangeControlReference,
                dateOfPreparation: LocalDate,
                timeOfPreparation: LocalTime,
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
    extends XmlBuilderHelper
    with MetaConstants {

  def toXml(messageCode: MessageCode): NodeSeq =
    buildAndEncodeElem(syntaxIdentifier, "SynIdeMES1") ++
      buildAndEncodeElem(syntaxVersionNumber, "SynVerNumMES2") ++
      messageSender.toXml ++
      buildOptionalElem(senderIdentificationCodeQualifier, "SenIdeCodQuaMES4") ++
      buildOptionalElem(recipientIdentificationCodeQualifier, "RecIdeCodQuaMES7") ++
      buildAndEncodeElem(messageRecipient, "MesRecMES6") ++
      buildAndEncodeElem(Format.dateFormatted(dateOfPreparation), "DatOfPreMES9") ++
      buildAndEncodeElem(Format.timeFormatted(timeOfPreparation), "TimOfPreMES10") ++
      interchangeControlReference.toXml ++
      buildOptionalElem(recipientsReferencePassword, "RecRefMES12") ++
      buildOptionalElem(recipientsReferencePasswordQualifier, "RecRefQuaMES13") ++
      buildAndEncodeElem(applicationReference, "AppRefMES14") ++
      buildOptionalElem(priority, "PriMES15") ++
      buildOptionalElem(acknowledgementRequest, "AckReqMES16") ++
      buildOptionalElem(communicationsAgreementId, "ComAgrIdMES17") ++
      buildAndEncodeElem(testIndicator, "TesIndMES18") ++
      buildAndEncodeElem(messageIndication, "MesIdeMES19") ++
      buildAndEncodeElem(messageCode.code, "MesTypMES20") ++
      buildOptionalElem(commonAccessReference, "ComAccRefMES21") ++
      buildOptionalElem(messageSequenceNumber, "MesSeqNumMES22") ++
      buildOptionalElem(firstAndLastTransfer, "FirAndLasTraMES23")

}

sealed trait MetaConstants {
  val syntaxIdentifier: String     = "UNOC"
  val syntaxVersionNumber: String  = "3"
  val messageRecipient: String     = "NCTS"
  val applicationReference: String = "NCTS"
  val messageIndication            = "1"
  val testIndicator                = "0"
}
