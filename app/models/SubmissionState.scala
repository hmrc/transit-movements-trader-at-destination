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

package models

trait SubmissionState {
  val message: String
  val monitorMessage: String
}

sealed trait PositiveState extends SubmissionState {
  override val monitorMessage: String = "success"
}

case class SubmissionSuccess(message: String) extends PositiveState

sealed trait InternalError extends SubmissionState {
  override val monitorMessage: String = "failure-internal"
}

case class FailedToLock(message: String)          extends InternalError
case class FailedToUnlock(message: String)        extends InternalError
case class FailedToSaveMessage(message: String)   extends InternalError
case class FailedToCreateMessage(message: String) extends InternalError

sealed trait ExternalError extends SubmissionState {
  override val monitorMessage: String = "failure-external"
}

case class ArrivalNotFoundError(message: String)        extends ExternalError
case class InvalidArrivalRootNodeError(message: String) extends ExternalError
case class TransitionError(message: String)             extends ExternalError
case class OutboundMessageError(message: String)        extends ExternalError
case class InboundMessageError(message: String)         extends ExternalError
case class DocumentExistsError(message: String)         extends ExternalError
case class CannotFindRootNodeError(message: String)     extends ExternalError
case class FailedToValidateMessage(message: String)     extends ExternalError
case class FailedToRetrieveMessage(message: String)     extends ExternalError
case class FailedToMakeMovementMessage(message: String) extends ExternalError
case class MessageSenderError(message: String)          extends ExternalError

sealed trait RejectionState extends SubmissionState {
  override val monitorMessage: String = "rejected"
}

case class SubmissionFailureRejected(message: String) extends RejectionState
