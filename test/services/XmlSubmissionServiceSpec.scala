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

import base.SpecBase
import connectors.MessageConnector
import generators.MessageGenerators
import models.messages.request._
import org.mockito.Matchers._
import org.mockito.Mockito.{reset, _}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import repositories.ArrivalNotificationRepository
import play.api.mvc.Results.NoContent
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Node

class XmlSubmissionServiceSpec extends SpecBase with BeforeAndAfterEach with ScalaCheckPropertyChecks with MessageGenerators {

  private val mockSubmissionModelService: SubmissionModelService = mock[SubmissionModelService]
  private val mockXmlBuilderService: XmlBuilderService = mock[XmlBuilderService]
  private val mockXmlValidationService: XmlValidationService = mock[XmlValidationService]
  private val mockMessageConnector: MessageConnector = mock[MessageConnector]
  private val mockArrivalNotificationRepository = mock[ArrivalNotificationRepository]

  private val application = {
    applicationBuilder
      .overrides(bind[SubmissionModelService].toInstance(mockSubmissionModelService))
      .overrides(bind[XmlBuilderService].toInstance(mockXmlBuilderService))
      .overrides(bind[XmlValidationService].toInstance(mockXmlValidationService))
      .overrides(bind[MessageConnector].toInstance(mockMessageConnector))
      .overrides(bind[ArrivalNotificationRepository].toInstance(mockArrivalNotificationRepository))
      .build()
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockSubmissionModelService)
    reset(mockXmlBuilderService)
    reset(mockXmlValidationService)
    reset(mockMessageConnector)
    reset(mockArrivalNotificationRepository)
  }

  private val submissionService: XmlSubmissionService = application.injector.instanceOf[XmlSubmissionService]
  private val interchangeControlReference: InterchangeControlReference = InterchangeControlReference("", 1)

  "Submit" - {

    "must return a RequestModelError when submission model conversion fails" in {

      when(mockSubmissionModelService.convertFromArrivalNotification(any(), any(), any()))
        .thenReturn(Left(FailedToConvert))

      submissionService.buildAndValidateXml(normalNotification, interchangeControlReference) mustBe Left(FailedToConvert)
    }

    "must return a RequestModelError when xml builder fails" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>

          when(mockSubmissionModelService.convertFromArrivalNotification(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlBuilderService.buildXml(any())(any()))
            .thenReturn(Left(FailedToCreateXml))

          submissionService.buildAndValidateXml(normalNotification, interchangeControlReference) mustBe Left(FailedToCreateXml)
      }
    }

    "must return a RequestModelError when xml validation fails" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>

          when(mockSubmissionModelService.convertFromArrivalNotification(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlBuilderService.buildXml(any())(any()))
            .thenReturn(Right(invalidNode))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Left(FailedToValidateXml))

          submissionService.buildAndValidateXml(normalNotification, interchangeControlReference) mustBe Left(FailedToValidateXml)
      }
    }

    "must return a Ok on successful post" in {

      forAll(arbitrary[ArrivalNotificationRequest]) {

        arrivalNotificationRequest =>

          when(mockSubmissionModelService.convertFromArrivalNotification(any(), any(), any()))
            .thenReturn(Right(arrivalNotificationRequest))

          when(mockXmlBuilderService.buildXml(any())(any()))
            .thenReturn(Right(validNode))

          when(mockXmlValidationService.validate(any(), any()))
            .thenReturn(Right((): Unit))

          when(mockMessageConnector.post(any(), any(), any())(any(), any()))
            .thenReturn(Future.successful(NoContent))

          val result = submissionService.buildAndValidateXml(normalNotification, interchangeControlReference).right.toOption.value

            result mustBe a[Node]

      }
    }

  }


  private val validNode: Node = {
    <CC007A>
      <SynIdeMES1>UNOC</SynIdeMES1>
      <SynVerNumMES2>3</SynVerNumMES2>
      <MesSenMES3>SYST17B-NCTS_EU_EXIT</MesSenMES3>
      <MesRecMES6>NCTS</MesRecMES6>
      <DatOfPreMES9>20190912</DatOfPreMES9>
      <TimOfPreMES10>1445</TimOfPreMES10>
      <IntConRefMES11>WE190912102534</IntConRefMES11>
      <AppRefMES14>NCTS</AppRefMES14>
      <TesIndMES18>0</TesIndMES18>
      <MesIdeMES19>1</MesIdeMES19>
      <MesTypMES20>GB007A</MesTypMES20>
      <HEAHEA>
        <DocNumHEA5>19IT02110010007827</DocNumHEA5>
        <ArrNotPlaHEA60>DOVER</ArrNotPlaHEA60>
        <SimProFlaHEA132>0</SimProFlaHEA132>
        <ArrNotDatHEA141>20191110</ArrNotDatHEA141>
      </HEAHEA>
      <TRADESTRD>
        <TINTRD59>GB163910077000</TINTRD59>
      </TRADESTRD>
      <CUSOFFPREOFFRES>
        <RefNumRES1>GB000060</RefNumRES1>
      </CUSOFFPREOFFRES>
    </CC007A>
  }

  private val invalidNode: Node = {
    <CC007A></CC007A>
  }

}
