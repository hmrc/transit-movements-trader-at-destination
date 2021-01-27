/*
 * Copyright 2021 HM Revenue & Customs
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

package metrics

import connectors.MessageConnector.EisSubmissionResult
import models.Arrival
import models.ChannelType
import models.MessageType
import models.SubmissionProcessingResult
import play.api.libs.ws.WSResponse

trait RequestMonitor[A] {
  val name: String

  val path: String         = s"$name.request"
  val timer: Timer         = Timer(path)
  val callCounter: Counter = Counter(path)

  def completionCounter(result: A): Option[Counter] = None
  val failureCounter: Counter                       = Counter(s"$path.failed")
}

case class EisRequestMonitor(name: String) extends RequestMonitor[EisSubmissionResult] {
  override def completionCounter(result: EisSubmissionResult): Option[Counter] =
    Some(Counter(s"$path.responseStatus.${result.statusCode}"))
}

case class WSResponseMonitor(name: String) extends RequestMonitor[WSResponse] {
  override def completionCounter(result: WSResponse): Option[Counter] =
    Some(Counter(s"$path.responseStatus.${result.status}"))
}

case class DefaultRequestMonitor[A](name: String) extends RequestMonitor[A]

object Monitors {
  val UnloadingPermissionMonitor: WSResponseMonitor                  = WSResponseMonitor("get-unloading-permission")
  val PostToEisMonitor: EisRequestMonitor                            = EisRequestMonitor("post-message-to-eis")
  val GetArrivalsForEoriMonitor: DefaultRequestMonitor[Seq[Arrival]] = DefaultRequestMonitor[Seq[Arrival]]("get-arrivals-for-eori")
  val GetArrivalByIdMonitor: DefaultRequestMonitor[Option[Arrival]]  = DefaultRequestMonitor[Option[Arrival]]("get-arrival-by-arrival-id")
  val GetArrivalByMrnMonitor: DefaultRequestMonitor[Option[Arrival]] = DefaultRequestMonitor[Option[Arrival]]("get-arrival-by-mrn")

  def arrivalsPerEori(arrivals: Seq[Arrival]): Counter =
    arrivals.size match {
      case s if s == 0   => Counter("arrivals-per-eori-0")
      case s if s <= 10  => Counter("arrivals-per-eori-1-10")
      case s if s <= 25  => Counter("arrivals-per-eori-11-25")
      case s if s <= 50  => Counter("arrivals-per-eori-26-50")
      case s if s <= 100 => Counter("arrivals-per-eori-51-100")
      case s if s <= 250 => Counter("arrivals-per-eori-101-250")
      case s if s <= 500 => Counter("arrivals-per-eori-251-500")
      case _             => Counter("arrivals-per-eori-501-or-more")
    }

  def countMessages(messageType: MessageType, channel: ChannelType, outcome: SubmissionProcessingResult): Counter =
    Counter(s"message-received.${channel.toString}.${messageType.code}-${outcome.toString}")
}
