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

import java.util.concurrent.atomic.AtomicBoolean

import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.Timer.Context
import com.kenshoo.play.metrics.Metrics
import javax.inject.Inject

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

class MetricsService @Inject()(metrics: Metrics) {

  private val registry: MetricRegistry = metrics.defaultRegistry

  private def startTimer(metric: Timer): Context = registry.timer(metric.path).time()

  def mark(metric: Meter): Unit  = registry.meter(metric.path).mark()
  def inc(metric: Counter): Unit = registry.counter(metric.path).inc()
  def dec(metric: Counter): Unit = registry.counter(metric.path).dec()

  def timeAsyncCall[A](monitor: RequestMonitor[A])(block: => Future[A])(implicit ec: ExecutionContext): Future[A] = {
    inc(monitor.callCounter)
    val timerContext: Context       = startTimer(monitor.timer)
    val timerRunning: AtomicBoolean = new AtomicBoolean(true)

    try {
      val result = block

      result.foreach {
        result =>
          if (timerRunning.compareAndSet(true, false)) {
            timerContext.stop()
            monitor.completionCounter(result).foreach(inc)
          }
      }

      result.failed.foreach {
        _ =>
          if (timerRunning.compareAndSet(true, false)) {
            timerContext.stop()
            inc(monitor.failureCounter)
          }
      }

      result

    } catch {
      case NonFatal(e) =>
        if (timerRunning.compareAndSet(true, false)) {
          timerContext.stop()
          inc(monitor.failureCounter)
        }
        throw e
    }
  }
}

case class Meter(name: String)   { val path = s"$name.rate"    }
case class Timer(name: String)   { val path = s"$name.timer"   }
case class Counter(name: String) { val path = s"$name.counter" }
