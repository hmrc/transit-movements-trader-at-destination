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

package models

import java.time.LocalDateTime

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageType._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.xml.NodeSeq

class ArrivalSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  val arrivalGenerator: Gen[Arrival] =
    for {
      messages <- nonEmptyListOfMaxLength[MovementMessageWithStatus](20)
      arrival  <- arbitrary[Arrival].map(_.copy(messages = messages))
    } yield arrival

  "nextMessageId returns a MessageId which has value that is 1 larger than the number of messages" in {
    forAll(arrivalGenerator) {
      arrival =>
        (MessageId.unapply(arrival.nextMessageId).value - arrival.messages.length) mustEqual 1
    }
  }

  "messageWithId returns a list with the message and its corresponding message ID" in {
    forAll(arrivalGenerator) {
      arrival =>
        arrival.messagesWithId.zipWithIndex.toList.foreach {
          case ((message, messageId), index) =>
            message mustEqual arrival.messages.toList(index)
            messageId mustEqual message.messageId
        }
    }
  }

  "latestMessageType" - {
    "when there is only the message from the user" - {

      "must return messageType" in {
        val localDateTime: LocalDateTime = LocalDateTime.now()

        val arrival =
          createArrival(
            22,
            localDateTime,
            List(
              createMessageMovement(22, ArrivalNotification, localDateTime.minusSeconds(10))
            )
          )

        arrival.latestMessageType mustBe ArrivalNotification
      }
    }

    "when there are responses from NCTS for the arrival" - {
      "when there is a single response from NCTS" - {
        "must return the messageType for the latest NCTS message" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val arrival =
            createArrival(
              22,
              localDateTime,
              List(
                createMessageMovement(22, ArrivalNotification, localDateTime.minusSeconds(10)),
                createMessageMovement(22, UnloadingPermission, localDateTime)
              )
            )

          arrival.latestMessageType mustBe UnloadingPermission
        }
      }

      "when there are multiple responses from NCTS" - {
        "when messages are well ordered" - {
          "must return the messageType for the latest NCTS message" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val arrival =
              createArrival(
                22,
                localDateTime,
                List(
                  createMessageMovement(22, ArrivalNotification, localDateTime.minusSeconds(20)),
                  createMessageMovement(22, GoodsReleased, localDateTime.minusSeconds(10)),
                  createMessageMovement(22, ArrivalRejection, localDateTime)
                )
              )

            arrival.latestMessageType mustBe ArrivalRejection
          }
        }

        "when messages are not well ordered" - {
          "must return the messageType for the message with the latest dateTime" - {

            "Scenario 1" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, ArrivalNotification, localDateTime.minusSeconds(20)),
                    createMessageMovement(22, ArrivalRejection, localDateTime.minusSeconds(15)),
                    createMessageMovement(22, GoodsReleased, localDateTime),
                    createMessageMovement(22, ArrivalNotification, localDateTime.minusSeconds(10))
                  )
                )

              arrival.latestMessageType mustBe GoodsReleased
            }

            "Scenario 2" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, ArrivalNotification, localDateTime.minusDays(3)),
                    createMessageMovement(22, UnloadingPermission, localDateTime.minusDays(2)),
                    createMessageMovement(22, UnloadingRemarks, localDateTime.minusDays(4))
                  )
                )

              arrival.latestMessageType mustBe UnloadingPermission
            }

            "Scenario 3" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, GoodsReleased, localDateTime),
                    createMessageMovement(22, ArrivalNotification, localDateTime.minusWeeks(2))
                  )
                )

              arrival.latestMessageType mustBe GoodsReleased
            }
          }
        }

        "when messages have the same latest dateTime" - {

          "and the current weighted message is ArrivalRejection" - {

            "must return ArrivalRejection when there are equal ArrivalNotification" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, ArrivalNotification, localDateTime),
                    createMessageMovement(22, ArrivalRejection, localDateTime)
                  )
                )

              arrival.latestMessageType mustBe ArrivalRejection
            }

            "must return ArrivalNotification when there are less ArrivalRejections" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, ArrivalNotification, localDateTime),
                    createMessageMovement(22, ArrivalRejection, localDateTime.plusSeconds(10)),
                    createMessageMovement(22, ArrivalNotification, localDateTime.plusSeconds(10))
                  )
                )

              arrival.latestMessageType mustBe ArrivalNotification
            }
          }

          "and the current weighted message is UnloadingRemarksRejection" - {

            "must return UnloadingRemarksRejection when there are equal UnloadingRemarks" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, UnloadingRemarksRejection, localDateTime),
                    createMessageMovement(22, UnloadingRemarks, localDateTime)
                  )
                )

              arrival.latestMessageType mustBe UnloadingRemarksRejection
            }

            "must return UnloadingRemarks when there are less UnloadingRemarksRejection" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val arrival =
                createArrival(
                  22,
                  localDateTime,
                  List(
                    createMessageMovement(22, UnloadingRemarks, localDateTime),
                    createMessageMovement(22, UnloadingRemarksRejection, localDateTime.plusSeconds(10)),
                    createMessageMovement(22, UnloadingRemarks, localDateTime.plusSeconds(10))
                  )
                )

              arrival.latestMessageType mustBe UnloadingRemarks
            }

          }

          "and the current weighted message is XMLSubmissionNegativeAcknowledgement" - {

            "and there are previous unloading remarks" - {

              "must return XMLSubmissionNegativeAcknowledgement when there are equal UnloadingRemarks" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val arrival =
                  createArrival(
                    22,
                    localDateTime,
                    List(
                      createMessageMovement(22, XMLSubmissionNegativeAcknowledgement, localDateTime),
                      createMessageMovement(22, UnloadingRemarks, localDateTime)
                    )
                  )

                arrival.latestMessageType mustBe XMLSubmissionNegativeAcknowledgement
              }

              "must return UnloadingRemarks when there are less XMLSubmissionNegativeAcknowledgement" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val arrival =
                  createArrival(
                    22,
                    localDateTime,
                    List(
                      createMessageMovement(22, UnloadingRemarks, localDateTime),
                      createMessageMovement(22, XMLSubmissionNegativeAcknowledgement, localDateTime.plusSeconds(10)),
                      createMessageMovement(22, UnloadingRemarks, localDateTime.plusSeconds(10))
                    )
                  )

                arrival.latestMessageType mustBe UnloadingRemarks
              }
            }

            "and there are previous arrivals ArrivalNotification" - {

              "must return XMLSubmissionNegativeAcknowledgement when there are equal ArrivalNotification" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val arrival =
                  createArrival(
                    22,
                    localDateTime,
                    List(
                      createMessageMovement(22, ArrivalNotification, localDateTime),
                      createMessageMovement(22, XMLSubmissionNegativeAcknowledgement, localDateTime)
                    )
                  )

                arrival.latestMessageType mustBe XMLSubmissionNegativeAcknowledgement
              }

              "must return ArrivalNotification when there are less XMLSubmissionNegativeAcknowledgement" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val arrival =
                  createArrival(
                    22,
                    localDateTime,
                    List(
                      createMessageMovement(22, ArrivalNotification, localDateTime),
                      createMessageMovement(22, XMLSubmissionNegativeAcknowledgement, localDateTime.plusSeconds(10)),
                      createMessageMovement(22, ArrivalNotification, localDateTime.plusSeconds(10))
                    )
                  )

                arrival.latestMessageType mustBe ArrivalNotification
              }

            }
          }

          "must return the latest messageType" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val arrival =
              createArrival(
                22,
                localDateTime,
                List(
                  createMessageMovement(22, ArrivalNotification, localDateTime.minusSeconds(20)),
                  createMessageMovement(22, GoodsReleased, localDateTime.minusSeconds(10)),
                  createMessageMovement(22, ArrivalRejection, localDateTime.minusSeconds(10))
                )
              )

            arrival.latestMessageType mustBe GoodsReleased
          }
        }
      }
    }
  }

  def createMessageMovement(messageId: Int, messageType: MessageType, localDateTime: LocalDateTime): MovementMessage =
    MovementMessageWithoutStatus(
      messageId = MessageId(messageId),
      dateTime = localDateTime,
      messageType = messageType,
      message = NodeSeq.Empty,
      messageCorrelationId = messageId
    )

  def createArrival(arrivalId: Int, localDateTime: LocalDateTime, messages: List[MovementMessage]): Arrival =
    Arrival(
      arrivalId = ArrivalId(arrivalId),
      channel = ChannelType.web,
      movementReferenceNumber = MovementReferenceNumber(arrivalId.toString),
      eoriNumber = arrivalId.toString,
      created = localDateTime,
      updated = localDateTime,
      lastUpdated = localDateTime,
      messages = NonEmptyList(messages.head, messages.tail),
      nextMessageCorrelationId = arrivalId + 1,
      notificationBox = None
    )
}
