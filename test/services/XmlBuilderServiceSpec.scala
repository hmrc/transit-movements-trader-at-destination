/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.LocalDateTime

import config.AppConfig
import generators.MessageGenerators
import models.messages.request.ArrivalNotificationRequest
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.Injector
import utils.Format

import scala.xml.Utility.trim
import scala.xml.{Node, NodeSeq}

class XmlBuilderServiceSpec extends FreeSpec
  with MustMatchers with GuiceOneAppPerSuite with MessageGenerators with ScalaCheckDrivenPropertyChecks with OptionValues {

  def injector: Injector = app.injector

  def appConfig: AppConfig = injector.instanceOf[AppConfig]

  val convertToXml = new XmlBuilderService()

  "XmlBuilderService" - {

    "must return correct nodes" in {

      def hasEoriWithNormalProcedure()(implicit arrivalNotificationRequest: ArrivalNotificationRequest): Boolean = {
        arrivalNotificationRequest.traderDestination.eori.isDefined &&
          arrivalNotificationRequest.header.simplifiedProcedureFlag.equals("0")
      }

      val localDateTime: Gen[LocalDateTime] = dateTimesBetween(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.now)

      forAll(arbitrary[ArrivalNotificationRequest](arbitraryArrivalNotificationRequest), localDateTime) {

        (arrivalNotificationRequest, dateTime) =>

          whenever(condition = hasEoriWithNormalProcedure()(arrivalNotificationRequest)) {

            val dateOfPreperation = Format.dateFormatted(dateTime)
            val timeOfPreperation = Format.timeFormatted(dateTime)

            val validXml: Node = {
              trim(
                <CC007A
                xsi:schemaLocation="http://ncts.dgtaxud.ec/CC007A"
                xmlns="http://ncts.dgtaxud.ec/CC007A"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:complex_ncts="http://ncts.dgtaxud.ec/complex_ncts">
                  <SynIdeMES1>
                    {arrivalNotificationRequest.syntaxIdentifier}
                  </SynIdeMES1>
                  <SynVerNumMES2>
                    {arrivalNotificationRequest.meta.syntaxVersionNumber}
                  </SynVerNumMES2>
                  <MesSenMES3>
                    {arrivalNotificationRequest.meta.messageSender.toString}
                  </MesSenMES3>
                  <MesRecMES6>
                    {arrivalNotificationRequest.meta.messageRecipient}
                  </MesRecMES6>
                  <DatOfPreMES9>
                    {dateOfPreperation}
                  </DatOfPreMES9>
                  <TimOfPreMES10>
                    {timeOfPreperation}
                  </TimOfPreMES10>
                  <IntConRefMES11>
                    {arrivalNotificationRequest.meta.interchangeControlReference.toString}
                  </IntConRefMES11>
                  <AppRefMES14>
                    {arrivalNotificationRequest.meta.applicationReference}
                  </AppRefMES14>
                  <MesIdeMES18>
                    {arrivalNotificationRequest.meta.testIndicator}
                  </MesIdeMES18>
                  <MesIdeMES19>
                    {arrivalNotificationRequest.meta.messageIndication}
                  </MesIdeMES19>
                  <MesTypMES20>
                    {arrivalNotificationRequest.messageCode}
                  </MesTypMES20>
                  <HEAHEA>
                    <DocNumHEA5>
                      {arrivalNotificationRequest.header.movementReferenceNumber}
                    </DocNumHEA5>{arrivalNotificationRequest.header.customsSubPlace.map {
                    customsSubPlace =>
                      <CusSubPlaHEA66>
                        {customsSubPlace}
                      </CusSubPlaHEA66>
                  }.getOrElse(NodeSeq.Empty)}<ArrNotPlaHEA60>
                    {arrivalNotificationRequest.header.arrivalNotificationPlace}
                  </ArrNotPlaHEA60>
                    <ArrNotPlaHEA60LNG>
                      {arrivalNotificationRequest.header.languageCode}
                    </ArrNotPlaHEA60LNG>
                    <ArrAgrLocOfGooHEA63LNG>
                      {arrivalNotificationRequest.header.languageCode}
                    </ArrAgrLocOfGooHEA63LNG>
                    <ArrivalAgreedLocationOfGoodsLNG>
                      {arrivalNotificationRequest.header.languageCode}
                    </ArrivalAgreedLocationOfGoodsLNG>
                    <SimProFlaHEA132>
                      {arrivalNotificationRequest.header.simplifiedProcedureFlag}
                    </SimProFlaHEA132>
                    <ArrNotDatHEA141>
                      {dateOfPreperation}
                    </ArrNotDatHEA141>
                  </HEAHEA>
                  <TRADESTRD>
                    {arrivalNotificationRequest.traderDestination.name.map {
                    name =>
                      <NamTRD7>
                        {name}
                      </NamTRD7>
                  }.getOrElse(NodeSeq.Empty)}{arrivalNotificationRequest.traderDestination.streetAndNumber.map {
                    streetAndNumber =>
                      <StrAndNumTRD22>
                        {streetAndNumber}
                      </StrAndNumTRD22>
                  }.getOrElse(NodeSeq.Empty)}{arrivalNotificationRequest.traderDestination.postCode.map {
                    postCode =>
                      <PosCodTRD23>
                        {postCode}
                      </PosCodTRD23>
                  }.getOrElse(NodeSeq.Empty)}{arrivalNotificationRequest.traderDestination.city.map {
                    city =>
                      <CitTRD24>
                        {city}
                      </CitTRD24>
                  }.getOrElse(NodeSeq.Empty)}{arrivalNotificationRequest.traderDestination.countryCode.map {
                    countryCode =>
                      <CouTRD25>
                        {countryCode}
                      </CouTRD25>
                  }.getOrElse(NodeSeq.Empty)}
                    <NADLNGRD>
                      {arrivalNotificationRequest.header.languageCode}
                    </NADLNGRD>
                    {arrivalNotificationRequest.traderDestination.eori.map {
                    eori =>
                      <TINTRD59>
                        {eori}
                      </TINTRD59>
                  }.getOrElse(NodeSeq.Empty)}
                  </TRADESTRD>
                  <CUSOFFPREOFFRES>
                    <RefNumRES1>
                      {arrivalNotificationRequest.customsOfficeOfPresentation.presentationOffice}
                    </RefNumRES1>
                  </CUSOFFPREOFFRES>
                </CC007A>
              )
            }

            trim(convertToXml.buildXml(arrivalNotificationRequest)(dateTime).right.toOption.value) mustBe validXml
          }
      }
    }
  }

}