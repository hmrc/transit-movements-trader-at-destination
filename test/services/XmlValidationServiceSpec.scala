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
import models.MessageReceived.GoodsReleased
import models.MessageReceived.UnloadingPermission

class XmlValidationServiceSpec extends SpecBase {

  private val xmlValidationService: XmlValidationService = new XmlValidationService()

  "validate" - {

    "must return a success when given a valid xml" in {

      val unloadingPermissionWithoutSealsXml = <CC043A><SynIdeMES1>UNOC</SynIdeMES1>
        <SynVerNumMES2>3</SynVerNumMES2>
        <MesSenMES3>NTA.GB</MesSenMES3>
        <MesRecMES6>SYST17B-NCTS_EU_EXIT</MesRecMES6>
        <DatOfPreMES9>20190912</DatOfPreMES9>
        <TimOfPreMES10>1448</TimOfPreMES10>
        <IntConRefMES11>66390912144854</IntConRefMES11>
        <AppRefMES14>NCTS</AppRefMES14>
        <TesIndMES18>0</TesIndMES18>
        <MesIdeMES19>66390912144854</MesIdeMES19>
        <MesTypMES20>GB043A</MesTypMES20>
        <HEAHEA><DocNumHEA5>19IT02110010007827</DocNumHEA5>
          <TypOfDecHEA24>T1</TypOfDecHEA24>
          <CouOfDesCodHEA30>GB</CouOfDesCodHEA30>
          <CouOfDisCodHEA55>IT</CouOfDisCodHEA55>
          <ConIndHEA96>0</ConIndHEA96>
          <AccDatHEA158>20190912</AccDatHEA158>
          <TotNumOfIteHEA305>1</TotNumOfIteHEA305>
          <TotNumOfPacHEA306>1</TotNumOfPacHEA306>
          <TotGroMasHEA307>1000</TotGroMasHEA307>
        </HEAHEA>
        <TRAPRIPC1><NamPC17>Mancini Carriers</NamPC17>
          <StrAndNumPC122>90 Desio Way</StrAndNumPC122>
          <PosCodPC123>MOD 5JJ</PosCodPC123>
          <CitPC124>Modena</CitPC124>
          <CouPC125>IT</CouPC125>
          <TINPC159>IT444100201000</TINPC159>
        </TRAPRIPC1>
        <TRACONCO1><NamCO17>Mancini Carriers</NamCO17>
          <StrAndNumCO122>90 Desio Way</StrAndNumCO122>
          <PosCodCO123>MOD 5JJ</PosCodCO123>
          <CitCO124>Modena</CitCO124>
          <CouCO125>IT</CouCO125>
          <TINCO159>IT444100201000</TINCO159>
        </TRACONCO1>
        <TRACONCE1><NamCE17>Mancini Carriers</NamCE17>
          <StrAndNumCE122>90 Desio Way</StrAndNumCE122>
          <PosCodCE123>MOD 5JJ</PosCodCE123>
          <CitCE124>Modena</CitCE124>
          <CouCE125>IT</CouCE125>
          <TINCE159>IT444100201000</TINCE159>
        </TRACONCE1>
        <TRADESTRD><NamTRD7>The Luggage Carriers</NamTRD7>
          <StrAndNumTRD22>225 Suedopolish Yard,</StrAndNumTRD22>
          <PosCodTRD23>SS8 2BB</PosCodTRD23>
          <CitTRD24>,</CitTRD24>
          <CouTRD25>GB</CouTRD25>
        </TRADESTRD>
        <CUSOFFDEPEPT><RefNumEPT1>IT021100</RefNumEPT1>
        </CUSOFFDEPEPT>
        <CUSOFFPREOFFRES><RefNumRES1>GB000060</RefNumRES1>
        </CUSOFFPREOFFRES>
        <GOOITEGDS><IteNumGDS7>1</IteNumGDS7>
          <GooDesGDS23>Flowers</GooDesGDS23>
          <GroMasGDS46>1000</GroMasGDS46>
          <NetMasGDS48>999</NetMasGDS48>
          <PRODOCDC2><DocTypDC21>235</DocTypDC21>
            <DocRefDC23>Ref.</DocRefDC23>
          </PRODOCDC2>
          <PACGS2>
            <MarNumOfPacGS21>Ref.</MarNumOfPacGS21>
            <KinOfPacGS23>BX</KinOfPacGS23>
            <NumOfPacGS24>1</NumOfPacGS24>
          </PACGS2>
        </GOOITEGDS>
      </CC043A>

      val result = xmlValidationService.validate(unloadingPermissionWithoutSealsXml.toString, UnloadingPermission)

      result mustBe true
    }

    "must return a failure when given an xml with missing mandatory elements" in {

      val invalidXml = <messages></messages>

      val result = xmlValidationService.validate(invalidXml.toString, UnloadingPermission)

      result.isFailure mustBe true
    }

    "must return a failure when given an xml with invalid fields" in {

      val invalidXml = <messages>
          <message>
            <testString>Goods are ready to be released</testString>
            <testNumber>somestring</testNumber>
          </message>
        </messages>

      val result = xmlValidationService.validate(invalidXml.toString, UnloadingPermission)

      result.isFailure mustBe true
    }
  }

}
