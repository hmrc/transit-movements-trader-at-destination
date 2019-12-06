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
import models.ArrivalNotificationXSD

class XmlValidationServiceSpec extends SpecBase {

  private val xmlValidationService = new XmlValidationService

  "validate" - {
    "must be successful when validating a valid ArrivalNotification xml" in {
      val xml =
        """
          |<CC007A>
          |    <SynIdeMES1>UNOC</SynIdeMES1>
          |    <SynVerNumMES2>3</SynVerNumMES2>
          |    <MesSenMES3>SYST17B-NCTS_EU_EXIT</MesSenMES3>
          |    <MesRecMES6>NCTS</MesRecMES6>
          |    <DatOfPreMES9>20190912</DatOfPreMES9>
          |    <TimOfPreMES10>1445</TimOfPreMES10>
          |    <IntConRefMES11>WE190912102534</IntConRefMES11>
          |    <AppRefMES14>NCTS</AppRefMES14>
          |    <TesIndMES18>0</TesIndMES18>
          |    <MesIdeMES19>1</MesIdeMES19>
          |    <MesTypMES20>GB007A</MesTypMES20>
          |    <HEAHEA>
          |        <DocNumHEA5>19IT02110010007827</DocNumHEA5>
          |        <ArrNotPlaHEA60>DOVER</ArrNotPlaHEA60>
          |        <SimProFlaHEA132>0</SimProFlaHEA132>
          |        <ArrNotDatHEA141>20191110</ArrNotDatHEA141>
          |    </HEAHEA>
          |    <TRADESTRD>
          |        <TINTRD59>GB163910077000</TINTRD59>
          |    </TRADESTRD>
          |    <CUSOFFPREOFFRES>
          |        <RefNumRES1>GB000060</RefNumRES1>
          |    </CUSOFFPREOFFRES>
          |</CC007A>
        """.stripMargin

      xmlValidationService.validate(xml, ArrivalNotificationXSD) mustBe a[Right[_, _]]
    }

    "must fail when validating a ArrivalNotification xml with missing elements" in {
      val xml =
        """
          |<CC007A>
          |    <HEAHEA>
          |        <DocNumHEA5>19IT02110010007827</DocNumHEA5>
          |        <ArrNotPlaHEA60>DOVER</ArrNotPlaHEA60>
          |        <SimProFlaHEA132>0</SimProFlaHEA132>
          |        <ArrNotDatHEA141>20191110</ArrNotDatHEA141>
          |    </HEAHEA>
          |    <TRADESTRD>
          |        <TINTRD59>GB163910077000</TINTRD59>
          |    </TRADESTRD>
          |    <CUSOFFPREOFFRES>
          |        <RefNumRES1>GB000060</RefNumRES1>
          |    </CUSOFFPREOFFRES>
          |</CC007A>
        """.stripMargin

      val expectedMessage = "cvc-complex-type.2.4.a: Invalid content was found starting with element 'HEAHEA'. One of '{SynIdeMES1}' is expected."

      xmlValidationService.validate(xml, ArrivalNotificationXSD) mustBe Left(FailedToValidateXml(expectedMessage))
    }

    "must fail when validating a ArrivalNotification xml with invalid fields" in {
      val xml =
        """
          |<CC007A>
          |    <SynIdeMES1>11111111111111</SynIdeMES1>
          |    <SynVerNumMES2>3</SynVerNumMES2>
          |    <MesSenMES3>SYST17B-NCTS_EU_EXIT</MesSenMES3>
          |    <MesRecMES6>NCTS</MesRecMES6>
          |    <DatOfPreMES9>20190912</DatOfPreMES9>
          |    <TimOfPreMES10>1445</TimOfPreMES10>
          |    <IntConRefMES11>WE190912102534</IntConRefMES11>
          |    <AppRefMES14>NCTS</AppRefMES14>
          |    <TesIndMES18>0</TesIndMES18>
          |    <MesIdeMES19>1</MesIdeMES19>
          |    <MesTypMES20>GB007A</MesTypMES20>
          |    <HEAHEA>
          |        <DocNumHEA5>19IT02110010007827</DocNumHEA5>
          |        <ArrNotPlaHEA60>DOVER</ArrNotPlaHEA60>
          |        <SimProFlaHEA132>0</SimProFlaHEA132>
          |        <ArrNotDatHEA141>20191110</ArrNotDatHEA141>
          |    </HEAHEA>
          |    <TRADESTRD>
          |        <TINTRD59>GB163910077000</TINTRD59>
          |    </TRADESTRD>
          |    <CUSOFFPREOFFRES>
          |        <RefNumRES1>GB000060</RefNumRES1>
          |    </CUSOFFPREOFFRES>
          |</CC007A>
        """.stripMargin

      val expectedMessage = "cvc-pattern-valid: Value '11111111111111' is not facet-valid with respect to pattern '[a-zA-Z]{4}' for type 'Alpha_4'."

      xmlValidationService.validate(xml, ArrivalNotificationXSD) mustBe Left(FailedToValidateXml(expectedMessage))
    }
  }

}
