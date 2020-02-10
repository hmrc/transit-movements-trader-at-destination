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

import java.time.LocalDateTime

import generators.MessageGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.Format

import scala.xml.NodeSeq

class MetaSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with MessageGenerators {

  private val genDateTime = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)
  //format off
  "Meta" - {

    val syntaxIdentifier     = "UNOC"
    val syntaxVersionNumber  = "3"
    val messageRecipient     = "NCTS"
    val applicationReference = "NCTS"
    val messageIndication    = "1"
    val testIndicator        = "0"

    "must create valid xml" in {
      forAll(arbitrary[Meta], genDateTime, arbitrary[String]) {
        (meta, dateTime, code) =>
          val messageCode = MessageCode(code)

          val senderIdentificationCodeQualifier = meta.senderIdentificationCodeQualifier.fold(NodeSeq.Empty)(
            senderIdentification => <SenIdeCodQuaMES4>{senderIdentification}</SenIdeCodQuaMES4>
          )

          val recipientIdentificationCodeQualifier =
            meta.recipientIdentificationCodeQualifier.fold(NodeSeq.Empty)(
              recipientIdentification => <RecIdeCodQuaMES7>{recipientIdentification}</RecIdeCodQuaMES7>
            )

          val recipientsReferencePassword = meta.recipientsReferencePassword.fold(NodeSeq.Empty)(
            recipientsReferencePassword => <RecRefMES12>{recipientsReferencePassword}</RecRefMES12>
          )

          val recipientsReferencePasswordQualifier = meta.recipientsReferencePasswordQualifier.fold(NodeSeq.Empty)(
            recipientsReferencePasswordQualifier => <RecRefQuaMES13>{recipientsReferencePasswordQualifier}</RecRefQuaMES13>
          )

          val priority = meta.priority.fold(NodeSeq.Empty)(
            priority => <PriMES15>{priority}</PriMES15>
          )

          val acknowledgementRequest = meta.acknowledgementRequest.fold(NodeSeq.Empty)(
            acknowledgementRequest => <AckReqMES16>{acknowledgementRequest}</AckReqMES16>
          )

          val communicationsAgreementId = meta.communicationsAgreementId.fold(NodeSeq.Empty)(
            communicationsAgreementId => <ComAgrIdMES17>{communicationsAgreementId}</ComAgrIdMES17>
          )

          val commonAccessReference = meta.commonAccessReference.fold(NodeSeq.Empty)(
            commonAccessReference => <ComAccRefMES21>{commonAccessReference}</ComAccRefMES21>
          )

          val messageSequenceNumber = meta.messageSequenceNumber.fold(NodeSeq.Empty)(
            messageSequenceNumber => <MesSeqNumMES22>{messageSequenceNumber}</MesSeqNumMES22>
          )

          val firstAndLastTransfer = meta.firstAndLastTransfer.fold(NodeSeq.Empty)(
            firstAndLastTransfer => <FirAndLasTraMES23>{firstAndLastTransfer}</FirAndLasTraMES23>
          )

          val expectedResult: NodeSeq = {

            <SynIdeMES1>{syntaxIdentifier}</SynIdeMES1> ++
              <SynVerNumMES2>{syntaxVersionNumber}</SynVerNumMES2> ++ {
              meta.messageSender.toXml ++
                senderIdentificationCodeQualifier ++
                recipientIdentificationCodeQualifier
            } ++
              <MesRecMES6>{messageRecipient}</MesRecMES6> ++
              <DatOfPreMES9>{Format.dateFormatted(dateTime)}</DatOfPreMES9> ++
              <TimOfPreMES10>{Format.timeFormatted(dateTime)}</TimOfPreMES10> ++ {
              meta.interchangeControlReference.toXml ++
                recipientsReferencePassword ++
                recipientsReferencePasswordQualifier
            } ++
              <AppRefMES14>{applicationReference}</AppRefMES14> ++ {
              priority ++
                acknowledgementRequest ++
                communicationsAgreementId
            } ++
              <TesIndMES18>{testIndicator}</TesIndMES18> ++
              <MesIdeMES19>{messageIndication}</MesIdeMES19> ++ {
              messageCode.toXml ++
                commonAccessReference ++
                messageSequenceNumber ++
                firstAndLastTransfer
            }
          }

          meta.toXml(messageCode)(dateTime) mustBe expectedResult
      }
    }
  }
  // format: on
}
