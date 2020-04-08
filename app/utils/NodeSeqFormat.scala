package utils

import play.api.libs.json.{JsError, JsResult, JsString, JsSuccess, JsValue, Reads, Writes}

import scala.xml.{NodeSeq, XML}

object NodeSeqFormat {
  implicit val writesNodeSeq: Writes[NodeSeq] = new Writes[NodeSeq] {
    override def writes(o: NodeSeq): JsValue = JsString(o.mkString)
  }

  implicit val readsNodeSeq: Reads[NodeSeq] = new Reads[NodeSeq] {
    override def reads(json: JsValue): JsResult[NodeSeq] = json match {
      case JsString(value) => JsSuccess(XML.loadString(value))
      case _               => JsError("Value cannot be parsed as XML")
    }
  }
}
