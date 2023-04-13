/*
 * Copyright 2023 HM Revenue & Customs
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

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import akka.util.ByteString
import base.SpecBase
import connectors.ManageDocumentsConnector
import generators.ModelGenerators
import models.WSError.NotFoundError
import models.WSError.OtherError
import models.response.ResponseMovementMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.libs.ws.ahc.AhcWSResponse
import play.api.test.Helpers._
import play.api.test.Helpers.running

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.NodeSeq

class UnloadingPermissionPDFServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks with BeforeAndAfterAll {
  implicit val system = ActorSystem(suiteName)

  override protected def afterAll(): Unit =
    TestKit.shutdownActorSystem(system)

  "UnloadingPermissionPDFService" - {

    val mockMessageRetrievalService   = mock[MessageRetrievalService]
    val mockManageDocumentConnector   = mock[ManageDocumentsConnector]
    val mockWSResponse: AhcWSResponse = mock[AhcWSResponse]

    when(mockManageDocumentConnector.getUnloadingPermissionPdf(any[NodeSeq])(any()))
      .thenReturn(Future.successful(mockWSResponse))

    "getPDF(Arrival)" - {

      "must return Left with OtherError when connector returns another result" in {

        val genErrorResponse = Gen.oneOf(300, 500)

        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], genErrorResponse) {
          (arrival, responseMovementMessage, errorCode) =>
            val expectedHeaders = Map(CONTENT_TYPE -> Seq("application/pdf"), CONTENT_DISPOSITION -> Seq("unloading_permission_123"))

            when(mockWSResponse.status) thenReturn errorCode
            when(mockWSResponse.body) thenReturn "Error message"
            when(mockWSResponse.headers) thenReturn expectedHeaders

            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.left.get mustBe OtherError(errorCode, "Error message")
            }
        }
      }

      "must return Right with a PDF" in {
        forAll(genArrivalWithSuccessfulArrival, arbitrary[ResponseMovementMessage], arbitrary[Array[Byte]]) {
          (arrival, responseMovementMessage, pdf) =>
            when(mockMessageRetrievalService.getUnloadingPermission(any()))
              .thenReturn(Some(responseMovementMessage))

            when(mockWSResponse.status) thenReturn 200
            when(mockWSResponse.bodyAsSource).thenAnswer(
              _ => Source.single(ByteString(pdf))
            )
            when(mockWSResponse.header(any[String])).thenAnswer(_.getArguments()(0) match {
              case CONTENT_TYPE        => Some("application/pdf")
              case CONTENT_DISPOSITION => Some("unloading_permission_123")
              case CONTENT_LENGTH      => Some(pdf.length.toString)
              case _                   => None
            })

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.right.get.dataSource.runWith(Sink.seq).futureValue must contain theSameElementsInOrderAs (Seq(ByteString(pdf)))
              result.right.get.contentLength mustBe Some(pdf.length)
              result.right.get.contentType mustBe Some("application/pdf")
              result.right.get.contentDisposition mustBe Some("unloading_permission_123")
            }
        }
      }

      "must return Left with a NotFoundError when UnloadingPermission is unavailable" in {
        forAll(genArrivalWithSuccessfulArrival) {
          arrival =>
            when(mockMessageRetrievalService.getUnloadingPermission(any())).thenReturn(None)

            val application = baseApplicationBuilder
              .overrides(bind[MessageRetrievalService].toInstance(mockMessageRetrievalService))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrival).futureValue

              result.left.get mustBe NotFoundError
            }
        }
      }
    }

    "getPDF(List[MovementMessage])" - {

      "must return Left with OtherError when connector returns another result" in {

        val genErrorResponse = Gen.oneOf(300, 500)

        forAll(genArrivalMessagesWithSpecificEori, genErrorResponse) {
          (arrivalMessages, errorCode) =>
            val expectedHeaders = Map(CONTENT_TYPE -> Seq("application/pdf"), CONTENT_DISPOSITION -> Seq("unloading_permission_123"))

            when(mockWSResponse.status) thenReturn errorCode
            when(mockWSResponse.body) thenReturn "Error message"
            when(mockWSResponse.headers) thenReturn expectedHeaders

            val application = baseApplicationBuilder
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrivalMessages.messages).futureValue

              result.left.get mustBe OtherError(errorCode, "Error message")
            }
        }
      }

      "must return Right with a PDF" in {
        forAll(genArrivalMessagesWithSpecificEori, arbitrary[Array[Byte]]) {
          (arrivalMessages, pdf) =>
            when(mockWSResponse.status) thenReturn 200
            when(mockWSResponse.bodyAsSource).thenAnswer(
              _ => Source.single(ByteString(pdf))
            )
            when(mockWSResponse.header(any[String])).thenAnswer(_.getArguments()(0) match {
              case CONTENT_TYPE        => Some("application/pdf")
              case CONTENT_DISPOSITION => Some("unloading_permission_123")
              case CONTENT_LENGTH      => Some(pdf.length.toString)
              case _                   => None
            })

            val application = baseApplicationBuilder
              .overrides(bind[ManageDocumentsConnector].toInstance(mockManageDocumentConnector))
              .build()

            running(application) {
              val service = application.injector.instanceOf[UnloadingPermissionPDFService]
              val result  = service.getPDF(arrivalMessages.messages).futureValue

              result.right.get.dataSource.runWith(Sink.seq).futureValue must contain theSameElementsInOrderAs (Seq(ByteString(pdf)))
              result.right.get.contentLength mustBe Some(pdf.length)
              result.right.get.contentType mustBe Some("application/pdf")
              result.right.get.contentDisposition mustBe Some("unloading_permission_123")
            }
        }
      }
    }
  }
}
