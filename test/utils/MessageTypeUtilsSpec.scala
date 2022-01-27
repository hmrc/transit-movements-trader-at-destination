/*
 * Copyright 2022 HM Revenue & Customs
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

import java.time.LocalDateTime
import base.SpecBase
import generators.ModelGenerators
import models.ArrivalStatus
import models.MessageId
import models.MessageType
import models.MovementMessage
import models.MovementMessageWithoutStatus
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.xml.NodeSeq

class MessageTypeUtilsSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "status" - {
    "when there is only the message from the user" - {

      "must return messageType" in {
        val localDateTime: LocalDateTime = LocalDateTime.now()

        val movementMessages =
          List(
            createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusSeconds(10))
          )

        MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalSubmitted
      }
    }

    "when there are responses from NCTS for the arrival" - {
      "when there is a single response from NCTS" - {
        "must return the messageType for the latest NCTS message" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val movementMessages =
            List(
              createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusSeconds(10)),
              createMessageMovement(22, MessageType.UnloadingPermission, localDateTime)
            )

          MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.UnloadingPermission
        }
      }

      "when there are multiple responses from NCTS" - {
        "when messages are well ordered" - {
          "must return the messageType for the latest NCTS message" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val movementMessages =
              List(
                createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusSeconds(20)),
                createMessageMovement(22, MessageType.GoodsReleased, localDateTime.minusSeconds(10)),
                createMessageMovement(22, MessageType.ArrivalRejection, localDateTime)
              )

            MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalRejected
          }
        }

        "when messages are not well ordered" - {
          "must return the messageType for the message with the latest dateTime" - {

            "Scenario 1" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusSeconds(20)),
                  createMessageMovement(22, MessageType.ArrivalRejection, localDateTime.minusSeconds(15)),
                  createMessageMovement(22, MessageType.GoodsReleased, localDateTime),
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusSeconds(10))
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.GoodsReleased
            }

            "Scenario 2" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusDays(3)),
                  createMessageMovement(22, MessageType.UnloadingPermission, localDateTime.minusDays(2)),
                  createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime.minusDays(4))
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.UnloadingPermission
            }

            "Scenario 3" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.GoodsReleased, localDateTime),
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusWeeks(2))
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.GoodsReleased
            }
          }
        }

        "when messages have the same latest dateTime" - {

          "and the current weighted message is ArrivalRejection" - {

            "must return ArrivalRejection when there are equal ArrivalNotification" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime),
                  createMessageMovement(22, MessageType.ArrivalRejection, localDateTime)
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalRejected
            }

            "must return ArrivalNotification when there are less ArrivalRejections" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime),
                  createMessageMovement(22, MessageType.ArrivalRejection, localDateTime.plusSeconds(10)),
                  createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.plusSeconds(10))
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalSubmitted
            }
          }

          "and the current weighted message is UnloadingRemarksRejection" - {

            "must return UnloadingRemarksRejection when there are equal UnloadingRemarks" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.UnloadingRemarksRejection, localDateTime),
                  createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime)
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.UnloadingRemarksRejected
            }

            "must return UnloadingRemarks when there are less UnloadingRemarksRejection" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime),
                  createMessageMovement(22, MessageType.UnloadingRemarksRejection, localDateTime.plusSeconds(10)),
                  createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime.plusSeconds(10))
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.UnloadingRemarksSubmitted
            }

          }

          "and the current weighted message is XMLSubmissionNegativeAcknowledgement" - {

            "must return NoValidStatus when there are no previous messages" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val movementMessages =
                List(
                  createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime),
                )

              MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.NoStatusFound
            }

            "and there are previous unloading remarks" - {

              "must return UnloadingRemarksSubmittedNegativeAcknowledgement when there are equal UnloadingRemarks" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime),
                    createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime)
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.UnloadingRemarksSubmittedNegativeAcknowledgement
              }

              "must return UnloadingRemarks when there are less XMLSubmissionNegativeAcknowledgement" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime),
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime.plusSeconds(10)),
                    createMessageMovement(22, MessageType.UnloadingRemarks, localDateTime.plusSeconds(10))
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.UnloadingRemarksSubmitted
              }
            }

            "and there are previous arrivals ArrivalNotification" - {

              "must return ArrivalSubmittedNegativeAcknowledgement when there are equal ArrivalNotification" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(22, MessageType.ArrivalNotification, localDateTime),
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime)
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalSubmittedNegativeAcknowledgement
              }

              "must return ArrivalNotification when there are less XMLSubmissionNegativeAcknowledgement" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(22, MessageType.ArrivalNotification, localDateTime),
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime.plusSeconds(10)),
                    createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.plusSeconds(10))
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalSubmitted
              }

            }
            "and there are previous arrivals XMLSubmissionNegativeAcknowledgement" - {

              "must return NoValidStatus when there are only XMLSubmissionNegativeAcknowledgement" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime.minusSeconds(10)),
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime)
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.NoStatusFound
              }
            }

            "and there are previous arrivals that aren't ArrivalNotification or UnloadingRemarks" - {

              "must return GoodsReleased when there are previous GoodsReleased" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(21, MessageType.GoodsReleased, localDateTime.minusSeconds(10)),
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime)
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.GoodsReleased
              }

              "must return ArrivalRejection when there are previous ArrivalRejection" in {

                val localDateTime: LocalDateTime = LocalDateTime.now()

                val movementMessages =
                  List(
                    createMessageMovement(21, MessageType.ArrivalRejection, localDateTime.minusSeconds(10)),
                    createMessageMovement(22, MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime)
                  )

                MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.ArrivalRejected
              }
            }

          }

          "must return the latest messageType" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val movementMessages =
              List(
                createMessageMovement(22, MessageType.ArrivalNotification, localDateTime.minusSeconds(20)),
                createMessageMovement(22, MessageType.GoodsReleased, localDateTime.minusSeconds(10)),
                createMessageMovement(22, MessageType.ArrivalRejection, localDateTime.minusSeconds(10))
              )

            MessageTypeUtils.status(movementMessages) mustBe ArrivalStatus.GoodsReleased
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

}
