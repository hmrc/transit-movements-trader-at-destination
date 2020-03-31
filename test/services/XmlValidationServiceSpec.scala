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

package services

import base.SpecBase
import models.GoodsReleasedXSD

class XmlValidationServiceSpec extends SpecBase {

  private val xmlValidationService = new XmlValidationService

  "validate" - {

    "when validating against a GoodsReleasedXSD" - {

      "must return a success when given a valid xml" in {

        val validXml = {
          <CC025A>
            <SynIdeMES1>UNOC</SynIdeMES1>
            <SynVerNumMES2>3</SynVerNumMES2>
            <MesSenMES3>NTA.GB</MesSenMES3>
            <MesRecMES6>SYST17B-NCTS_EU_EXIT</MesRecMES6>
            <DatOfPreMES9>20191004</DatOfPreMES9>
            <TimOfPreMES10>1000</TimOfPreMES10>
            <IntConRefMES11>25391004100052</IntConRefMES11>
            <AppRefMES14>NCTS</AppRefMES14>
            <TesIndMES18>0</TesIndMES18>
            <MesIdeMES19>25391004100052</MesIdeMES19>
            <MesTypMES20>GB025A</MesTypMES20>
            <HEAHEA>
              <DocNumHEA5>19IT02110010007A33</DocNumHEA5>
              <GooRelDatHEA176>20191004</GooRelDatHEA176>
            </HEAHEA>
            <TRADESTRD>
              <NamTRD7>QUIRM ENGINEERING</NamTRD7>
              <StrAndNumTRD22>125 Psuedopolis Yard</StrAndNumTRD22>
              <PosCodTRD23>SS99 1AA</PosCodTRD23>
              <CitTRD24>Ank-Morpork</CitTRD24>
              <CouTRD25>GB</CouTRD25>
              <TINTRD59>GB602070107000</TINTRD59>
            </TRADESTRD>
            <CUSOFFPREOFFRES>
              <RefNumRES1>GB000060</RefNumRES1>
            </CUSOFFPREOFFRES>
          </CC025A>
        }

        val result = xmlValidationService.validate(validXml.toString, GoodsReleasedXSD)

        result.isSuccess mustBe true
      }

      "must return a failure when given an xml with missing mandatory elements" in {

        val invalidXml = {
          <CC025A>
            <SynIdeMES1>UNOC</SynIdeMES1>
            <SynVerNumMES2>3</SynVerNumMES2>
            <MesSenMES3>NTA.GB</MesSenMES3>
            <MesRecMES6>SYST17B-NCTS_EU_EXIT</MesRecMES6>
            <DatOfPreMES9>20191004</DatOfPreMES9>
            <TimOfPreMES10>1000</TimOfPreMES10>
            <IntConRefMES11>25391004100052</IntConRefMES11>
            <AppRefMES14>NCTS</AppRefMES14>
            <TesIndMES18>0</TesIndMES18>
            <MesIdeMES19>25391004100052</MesIdeMES19>
            <MesTypMES20>GB025A</MesTypMES20>
          </CC025A>
        }

        val result = xmlValidationService.validate(invalidXml.toString, GoodsReleasedXSD)

        result.isFailure mustBe true
      }

      "must return a failure when given an xml with invalid fields" in {

        val invalidXml = {
          <CC025A>
            <SynIdeMES1>UNOC</SynIdeMES1>
            <SynVerNumMES2>3</SynVerNumMES2>
            <MesSenMES3>NTA.GB</MesSenMES3>
            <MesRecMES6>SYST17B-NCTS_EU_EXIT</MesRecMES6>
            <DatOfPreMES9>20191004</DatOfPreMES9>
            <TimOfPreMES10>Invalid field</TimOfPreMES10>
            <IntConRefMES11>25391004100052</IntConRefMES11>
            <AppRefMES14>NCTS</AppRefMES14>
            <TesIndMES18>0</TesIndMES18>
            <MesIdeMES19>25391004100052</MesIdeMES19>
            <MesTypMES20>GB025A</MesTypMES20>
            <HEAHEA>
              <DocNumHEA5>19IT02110010007A33</DocNumHEA5>
              <GooRelDatHEA176>20191004</GooRelDatHEA176>
            </HEAHEA>
            <TRADESTRD>
              <NamTRD7>QUIRM ENGINEERING</NamTRD7>
              <StrAndNumTRD22>125 Psuedopolis Yard</StrAndNumTRD22>
              <PosCodTRD23>SS99 1AA</PosCodTRD23>
              <CitTRD24>Ank-Morpork</CitTRD24>
              <CouTRD25>GB</CouTRD25>
              <TINTRD59>GB602070107000</TINTRD59>
            </TRADESTRD>
            <CUSOFFPREOFFRES>
              <RefNumRES1>GB000060</RefNumRES1>
            </CUSOFFPREOFFRES>
          </CC025A>
        }

        val result = xmlValidationService.validate(invalidXml.toString, GoodsReleasedXSD)

        result.isFailure mustBe true
      }
    }
  }

}
