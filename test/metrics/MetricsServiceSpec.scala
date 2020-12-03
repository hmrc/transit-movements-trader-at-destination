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

package metrics

import com.codahale.metrics.Timer.Context
import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.{Counter => KenshooCounter}
import com.codahale.metrics.{Timer => KenshooTimer}
import com.kenshoo.play.metrics.Metrics
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.never
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers.running

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MetricsServiceSpec extends AnyFreeSpec with Matchers with MockitoSugar with ScalaFutures {

  case class TestMonitor(name: String) extends RequestMonitor[Boolean] {
    override def completionCounter(result: Boolean): Option[Counter] = Some(Counter(s"$path.completion"))
  }

  ".timeAsyncCall" - {

    "when the block succeeds" - {

      "must increment the call and completion counters and time the request" in new Fixture {

        running(app) {

          val metricsService = app.injector.instanceOf[MetricsService]
          val monitor        = TestMonitor("foo")

          val result = metricsService
            .timeAsyncCall(monitor) {
              Future.successful(true)
            }

          whenReady(result) {
            _ =>
              verify(mockTimer, times(1)).time()
              verify(mockContext, times(1)).stop()
              verify(mockCallCounter, times(1)).inc()
              verify(mockCompletionCounter, times(1)).inc()
              verify(mockFailureCounter, never()).inc()
          }
        }
      }
    }

    "when the block returns a failed Future" - {

      "must increment the call and failure counters and time the request" in new Fixture {

        running(app) {

          val metricsService = app.injector.instanceOf[MetricsService]
          val monitor        = TestMonitor("foo")

          val result = metricsService
            .timeAsyncCall(monitor) {
              Future.failed(new Exception("foo"))
            }

          whenReady(result.failed) {
            _ =>
              verify(mockTimer, times(1)).time()
              verify(mockContext, times(1)).stop()
              verify(mockCallCounter, times(1)).inc()
              verify(mockCompletionCounter, never()).inc()
              verify(mockFailureCounter, times(1)).inc()
          }
        }
      }
    }

    "when the block throws an exception" - {

      "must increment the call and failure counters and time the request" in new Fixture {

        running(app) {

          val metricsService = app.injector.instanceOf[MetricsService]
          val monitor        = TestMonitor("foo")

          intercept[Exception] {
            metricsService
              .timeAsyncCall(monitor) {
                throw new Exception("foo")
              }
          }

          verify(mockTimer, times(1)).time()
          verify(mockContext, times(1)).stop()
          verify(mockCallCounter, times(1)).inc()
          verify(mockCompletionCounter, never()).inc()
          verify(mockFailureCounter, times(1)).inc()
        }
      }
    }
  }

  trait Fixture {
    protected val mockMetrics: Metrics                  = mock[Metrics]
    protected val mockMetricRegistry: MetricRegistry    = mock[MetricRegistry]
    protected val mockCallCounter: KenshooCounter       = mock[KenshooCounter]
    protected val mockCompletionCounter: KenshooCounter = mock[KenshooCounter]
    protected val mockFailureCounter: KenshooCounter    = mock[KenshooCounter]
    protected val mockTimer: KenshooTimer               = mock[KenshooTimer]
    protected val mockContext: Context                  = mock[Context]

    when(mockMetrics.defaultRegistry) thenReturn mockMetricRegistry
    when(mockMetricRegistry.counter("foo.request.counter")) thenReturn mockCallCounter
    when(mockMetricRegistry.counter("foo.request.completion.counter")) thenReturn mockCompletionCounter
    when(mockMetricRegistry.counter("foo.request.failed.counter")) thenReturn mockFailureCounter
    when(mockMetricRegistry.timer(any())) thenReturn mockTimer
    when(mockTimer.time()) thenReturn mockContext

    protected val app: Application =
      new GuiceApplicationBuilder().overrides(bind[Metrics].toInstance(mockMetrics)).build()
  }
}
