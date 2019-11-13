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

package utils

import java.io._
import java.net.URL

import javax.xml.validation.Schema
import org.xml.sax.InputSource
import org.xml.sax.helpers.DefaultHandler

import scala.util.{Failure, Success, Try}
import scala.xml.factory.XMLLoader
import scala.xml.{Elem, SAXException, SAXParseException, SAXParser}

/**
  * TODO: Below we were trying to implement functionality
  * to validate xml with and xsd document
  */

object ValidateXML {

  def validate(xml: String, fileKey: String): Try[String] = {

    val schemaLang = javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI
    val url: URL = getClass.getResource(s"/xsd-iconvert/$fileKey.xsd")

    val schema: Schema = javax.xml.validation.SchemaFactory.newInstance(schemaLang).newSchema(url)

    val factory = javax.xml.parsers.SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setSchema(schema)
    factory.setValidating(true)

    val validatingParser = factory.newSAXParser()

    class CustomParseHandler extends DefaultHandler {

      override def error(e: SAXParseException ): Unit = {

        if(
          !e.getMessage.contentEquals("Document is invalid: no grammar found.") &&
            !e.getMessage.contentEquals("Document root element \"CC007A\", must match DOCTYPE root \"null\".")
        )
        {
          println(s"EXCEPTION ${e.getException}")
          println(s"MESSAGE ${e.getMessage}")
          throw new SAXException(e)
        }
      }
    }

    val xmlResponse: XMLLoader[Elem] = new scala.xml.factory.XMLLoader[scala.xml.Elem] {
      override def parser: SAXParser = validatingParser
      override def adapter =
        new scala.xml.parsing.NoBindingFactoryAdapter
          with scala.xml.parsing.ConsoleErrorHandler

    }

    try {

      xmlResponse.parser.parse(new InputSource(new StringReader(xml)), new CustomParseHandler)

      Success("successfully parsed xml")

    } catch {
      case e: Throwable => Failure(e)
    }

  }

}
