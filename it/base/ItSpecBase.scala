package base

import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageStatus.SubmissionPending
import models.Arrival
import models.MovementMessageWithStatus
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.TryValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ItSpecBase
    extends AnyFreeSpec
    with Matchers
    with ScalaFutures
    with IntegrationPatience
    with OptionValues
    with EitherValues
    with TryValues
    with ModelGenerators {

  val arrivalWithOneMessage: Gen[Arrival] = for {
    arrival         <- arbitrary[Arrival]
    movementMessage <- arbitrary[MovementMessageWithStatus]
  } yield arrival.copy(messages = NonEmptyList.one(movementMessage.copy(status = SubmissionPending)))

}
