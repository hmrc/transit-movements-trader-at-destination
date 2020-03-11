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

import helpers.XmlBuilderHelper
import models.messages.EnRouteEvent

import scala.collection.immutable.ListMap
import scala.xml.Node
import scala.xml.NodeSeq

case class ArrivalNotificationRequest(meta: Meta,
                                      header: Header,
                                      traderDestination: TraderDestination,
                                      customsOfficeOfPresentation: CustomsOfficeOfPresentation,
                                      enRouteEvents: Option[Seq[EnRouteEvent]])
    extends XmlBuilderHelper
    with RequestConstants {

  val xMessageType: XMessageType     = XMessageType("IE007")
  val messageCode: MessageCode       = MessageCode("GB007A")
  val syntaxIdentifier: String       = "UNOC"
  val nameSpace: Map[String, String] = ListMap()

  def toXml: Node = {

    val parentNode: Node = <CC007A></CC007A>

    val childNodes: NodeSeq = {
      meta.toXml(messageCode) ++
        header.toXml ++
        traderDestination.toXml ++
        customsOfficeOfPresentation.toXml ++ {
        enRouteEvents.map(_.map(_.toXml)).getOrElse(NodeSeq.Empty)
      }
    }

    val arrivalNotificationRequestXml = addChildrenToRoot(parentNode, childNodes)

    arrivalNotificationRequestXml
  }
}
