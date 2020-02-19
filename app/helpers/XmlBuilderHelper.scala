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

package helpers

import java.time.LocalDate

import models.request._
import play.twirl.api.utils.StringEscapeUtils
import utils.Format

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.XML._

class XmlBuilderHelper {

  def buildAndEncodeElem[A](value: A, elementTag: String): NodeSeq = value match {
    case result: String => {
      val encodeResult = StringEscapeUtils.escapeXml11(result)
      loadString(s"<$elementTag>$encodeResult</$elementTag>")
    }
    case result: LocalDate         => loadString(s"<$elementTag>${Format.dateFormatted(result)}</$elementTag>")
    case result: Boolean           => loadString(s"<$elementTag>${if (result) 1 else 0}</$elementTag>")
    case result: LanguageCode      => loadString(s"<$elementTag>${result.code}</$elementTag>")
    case result: ProcedureTypeFlag => loadString(s"<$elementTag>${result.code}</$elementTag>")
    case _                         => NodeSeq.Empty
  }

  def buildOptionalElem[A](value: Option[A], elementTag: String): NodeSeq = value match {
    case Some(result) => buildAndEncodeElem(result, elementTag)
    case _            => NodeSeq.Empty
  }

  def addChildrenToRoot(root: Node, childNodes: NodeSeq): Node =
    Elem(root.prefix, root.label, root.attributes, root.scope, root.child.isEmpty, root.child ++ childNodes: _*)
}
