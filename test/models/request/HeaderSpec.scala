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

import generators.MessageGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.Format

import scala.xml.NodeSeq
import scala.xml.Utility.trim
import scala.xml.XML.loadString

class HeaderSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with MessageGenerators {

  private val genDateTime = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)

  "Header" - {
    "must create minimal valid xml" in {

      forAll(arbitrary[Header], genDateTime) {
        (header, arrivalNotificationDate) =>
          val minimalHeader = Header(
            movementReferenceNumber = header.movementReferenceNumber.toString,
            procedureTypeFlag = header.procedureTypeFlag,
            arrivalNotificationPlace = header.arrivalNotificationPlace
          )

          val expectedResult =
            <HEAHEA>
              <DocNumHEA5>{minimalHeader.movementReferenceNumber}</DocNumHEA5>
              <ArrNotPlaHEA60>{minimalHeader.arrivalNotificationPlace}</ArrNotPlaHEA60>
              <ArrNotPlaHEA60LNG>{LanguageCodeEnglish.code}</ArrNotPlaHEA60LNG>
              <ArrAgrLocOfGooHEA63LNG>{LanguageCodeEnglish.code}</ArrAgrLocOfGooHEA63LNG>
              <SimProFlaHEA132>{minimalHeader.procedureTypeFlag.code}</SimProFlaHEA132>
              <ArrNotDatHEA141>{Format.dateFormatted(arrivalNotificationDate)}</ArrNotDatHEA141>
            </HEAHEA>

          trim(minimalHeader.toXml(arrivalNotificationDate)) mustBe trim(loadString(expectedResult.toString))
      }
    }

    "must create valid xml" in {
      forAll(arbitrary[Header], genDateTime) {
        (header, arrivalNotificationDate) =>
          // TODO need to follow up on these elements as ArrAgrLocCodHEA62,
          //  ArrAgrLocCodHEA62 and ArrAutLocOfGooHEA65 are the same value

          val customsSubPlaceNode = header.customsSubPlace.map(
            customsSubPlace => <CusSubPlaHEA66>{customsSubPlace}</CusSubPlaHEA66>
          )
          val agreedLocationOfGoods = header.arrivalAgreedLocationOfGoods.map(
            arrivalAgreedLocationOfGoods => <ArrAgrLocCodHEA62>{arrivalAgreedLocationOfGoods}</ArrAgrLocCodHEA62>
              <ArrAgrLocOfGooHEA63>{arrivalAgreedLocationOfGoods}</ArrAgrLocOfGooHEA63>
          )
          val authorisedLocationOfGoods = header.arrivalAgreedLocationOfGoods.map(
            arrivalAgreedLocationOfGoods => <ArrAutLocOfGooHEA65>{arrivalAgreedLocationOfGoods}</ArrAutLocOfGooHEA65>
          )

          val expectedResult =
            <HEAHEA>
              <DocNumHEA5>{header.movementReferenceNumber}</DocNumHEA5>
              {customsSubPlaceNode.getOrElse(NodeSeq.Empty)}
              <ArrNotPlaHEA60>{header.arrivalNotificationPlace}</ArrNotPlaHEA60>
              <ArrNotPlaHEA60LNG>{LanguageCodeEnglish.code}</ArrNotPlaHEA60LNG>
              {agreedLocationOfGoods.getOrElse(NodeSeq.Empty)}
              <ArrAgrLocOfGooHEA63LNG>{LanguageCodeEnglish.code}</ArrAgrLocOfGooHEA63LNG>
              {authorisedLocationOfGoods.getOrElse(NodeSeq.Empty)}
              <SimProFlaHEA132>{header.procedureTypeFlag.code}</SimProFlaHEA132>
              <ArrNotDatHEA141>{Format.dateFormatted(arrivalNotificationDate)}</ArrNotDatHEA141>
            </HEAHEA>

          trim(header.toXml(arrivalNotificationDate)) mustBe trim(loadString(expectedResult.toString))
      }
    }
  }

}
