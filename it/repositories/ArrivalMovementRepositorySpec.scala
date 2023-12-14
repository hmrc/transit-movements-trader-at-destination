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

package repositories

import base._
import cats.data._
import com.kenshoo.play.metrics.Metrics
import com.mongodb.client.model.Filters
import config.AppConfig
import controllers.routes
import models.ChannelType.api
import models.ChannelType.web
import models._
import models.response._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.source
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions._
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import play.api._
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport
import utils.Format

import java.time._
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util._
import scala.xml.NodeSeq

class ArrivalMovementRepositorySpec
    extends ItSpecBase
    with ScalaFutures
    with TestSuiteMixin
    with Matchers
    with MongoDateTimeFormats
    with BeforeAndAfterEach
    with DefaultAwaitTimeout
    with FutureAwaits
    with MockitoSugar
    with DefaultPlayMongoRepositorySupport[Arrival] {

  private val instant                   = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  implicit private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  private val app: Application =
    GuiceApplicationBuilder()
      .overrides(bind[Clock].toInstance(stubClock))
      .configure(
        "metrics.jvm"                    -> false,
        "feature-flags.testOnly.enabled" -> true
      )
      .build

  private val config    = app.injector.instanceOf[Configuration]
  private val appConfig = app.injector.instanceOf[AppConfig]
  private val metrics   = app.injector.instanceOf[Metrics]

  val localDate      = LocalDate.now()
  val localTime      = LocalTime.of(1, 1)
  val localDateTime  = LocalDateTime.of(localDate, localTime)
  implicit val clock = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

  override val databaseName: String = "arrival-movements-hmrc-mongo"

  override lazy val repository =
    new ArrivalMovementRepositoryImpl(mongoComponent, appConfig, config, clock, metrics)(app.materializer.executionContext, app.materializer)

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult =>
      throw new TestFailedException(
        (_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
        None,
        implicitly[source.Position]
      )
  }

  def nonEmptyListOfNArrivals(size: Int): Gen[NonEmptyList[Arrival]] =
    Gen
      .listOfN(size, arbitrary[Arrival])
      // Don't generate duplicate IDs
      .map(_.foldLeft((Chain.empty[Arrival], 1)) {
        case ((arrivals, id), arrival) =>
          (arrivals :+ arrival.copy(arrivalId = ArrivalId(id)), id + 1)
      })
      .map {
        case (arrivals, _) =>
          NonEmptyList.fromListUnsafe(arrivals.toList)
      }

  private val eoriNumber: String = arbitrary[String].sample.value
  private val turn: String       = arbitrary[String].sample.value
  private val mrn                = arbitrary[MovementReferenceNumber].sample.value

  "ArrivalMovementRepository" - {

    "insert" - {
      "must persist ArrivalMovement within mongoDB" in {

        val arrivalArb = arbitrary[Arrival].sample.value
        val arrival    = arrivalArb.copy(eoriNumber = "1234567", movementReferenceNumber = MovementReferenceNumber("DP987654"))

        repository.insert(arrival).futureValue

        val selector = Filters.eq("eoriNumber", arrival.eoriNumber)
        val result   = repository.collection.find(selector).head()

        whenReady(result) {
          _ mustBe arrival
        }

      }
    }

    "getMaxArrivalId" - {
      "must return the highest arrival id in the database" in {

        val arrivals = List.tabulate(5)(
          index => arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(index + 1))
        )

        repository.bulkInsert(arrivals).futureValue

        repository.getMaxArrivalId.futureValue.value mustBe ArrivalId(5)
      }
    }

    "addResponseMessage" - {
      "must add a message, update the status of a document and update the timestamp" in {

        val arrival = arbitrary[Arrival].sample.value.copy(updated = localDateTime.minusDays(1), lastUpdated = localDateTime.minusDays(1))

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)

        val messageBody =
          <CC025A>
            <DatOfPreMES9>
              {Format.dateFormatted(dateOfPrep)}
            </DatOfPreMES9>
            <TimOfPreMES10>
              {Format.timeFormatted(timeOfPrep)}
            </TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            arrival.nextMessageCorrelationId
          )

        await(repository.insert(arrival))
        val addMessageResult = await(repository.addResponseMessage(arrival.arrivalId, goodsReleasedMessage))

        val selector = Filters.eq("_id", arrival.arrivalId)

        val updatedArrival = await(repository.collection.find(selector).head())

        addMessageResult mustBe a[Success[_]]
        updatedArrival.nextMessageCorrelationId - arrival.nextMessageCorrelationId mustBe 0
        updatedArrival.updated.toString mustEqual goodsReleasedMessage.received.get.toString
        updatedArrival.lastUpdated.toString mustEqual goodsReleasedMessage.received.get.toString
        updatedArrival.messages.size - arrival.messages.size mustEqual 1
        updatedArrival.messages.last.toString mustEqual goodsReleasedMessage.toString

      }

      "must fail if the arrival cannot be found" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)

        val messageBody =
          <CC025A>
            <DatOfPreMES9>
              {Format.dateFormatted(dateOfPrep)}
            </DatOfPreMES9>
            <TimOfPreMES10>
              {Format.timeFormatted(timeOfPrep)}
            </TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            messageCorrelationId = 1
          )

        repository.insert(arrival).futureValue

        val result = repository.addResponseMessage(ArrivalId(2), goodsReleasedMessage).futureValue
        result mustBe a[Failure[_]]
      }
    }

    "addNewMessage" - {
      "must add a message, update the timestamp and increment nextCorrelationId" in {

        val arrival = arbitrary[Arrival].sample.value

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)

        val messageBody =
          <CC025A>
            <DatOfPreMES9>
              {Format.dateFormatted(dateOfPrep)}
            </DatOfPreMES9>
            <TimOfPreMES10>
              {Format.timeFormatted(timeOfPrep)}
            </TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            arrival.nextMessageCorrelationId
          )

        repository.insert(arrival).futureValue

        repository.addNewMessage(arrival.arrivalId, goodsReleasedMessage).futureValue

        val selector = Filters.eq("_id", arrival.arrivalId)

        val updatedArrival = repository.collection.find(selector).head().futureValue

        updatedArrival.nextMessageCorrelationId - arrival.nextMessageCorrelationId mustBe 1
        updatedArrival.updated mustEqual goodsReleasedMessage.received.get
        updatedArrival.lastUpdated mustEqual goodsReleasedMessage.received.get
        updatedArrival.messages.size - arrival.messages.size mustEqual 1
        updatedArrival.messages.last.toString.stripMargin mustEqual goodsReleasedMessage.toString.stripMargin // TODO: not sure why not equal!
      }

      "must fail if the arrival cannot be found" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        val dateOfPrep = LocalDate.now(stubClock)
        val timeOfPrep = LocalTime.now(stubClock).withHour(1).withMinute(1)
        val messageBody =
          <CC025A>
            <DatOfPreMES9>
              {Format.dateFormatted(dateOfPrep)}
            </DatOfPreMES9>
            <TimOfPreMES10>
              {Format.timeFormatted(timeOfPrep)}
            </TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val goodsReleasedMessage =
          MovementMessageWithoutStatus(
            arrival.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.GoodsReleased,
            messageBody,
            messageCorrelationId = 1
          )

        repository.insert(arrival).futureValue
        val result = repository.addNewMessage(ArrivalId(2), goodsReleasedMessage).futureValue
        result mustBe a[Failure[_]]

      }
    }

    "get(arrivalId: ArrivalId)" - {
      "must get an departure when it exists and has the right channel type" in {

        val arrival = arbitrary[Arrival].sample.value

        repository.insert(arrival).futureValue
        val result = repository.get(arrival.arrivalId)

        whenReady(result) {
          r =>
            r.value mustEqual arrival
        }
      }

      "must return None when an departure does not exist" in {

        val arrival = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2))
        repository.insert(arrival).futureValue

        val result = repository.get(ArrivalId(1))
        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "get(arrivalId: ArrivalId, channelFilter: ChannelType)" - {
      "must get an arrival when it exists and has the right channel type" in {

        val arrival = arbitrary[Arrival].sample.value

        repository.insert(arrival).futureValue
        val result = repository.get(arrival.arrivalId, arrival.channel).futureValue

        result.value mustEqual arrival.copy(lastUpdated = result.value.lastUpdated)

      }

      "must return None when an arrival does not exist" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        repository.insert(arrival).futureValue
        val result = repository.get(ArrivalId(2), arrival.channel).futureValue

        result must not be defined

      }

      "must return None when an arrival does exist but with the wrong channel type" in {

        val arrival = arbitrary[Arrival].sample.value copy (channel = api)

        repository.insert(arrival).futureValue
        val result = repository.get(arrival.arrivalId, web).futureValue

        result must not be defined

      }
    }

    "getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType])" - {

      val mType = MessageType.UnloadingPermission

      val node = NodeSeq.fromSeq(Seq(<CC043A>m</CC043A>))

      val message = MovementMessageWithStatus(
        MessageId(1),
        LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS),
        Some(LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS)),
        MessageType.UnloadingPermission,
        node,
        MessageStatus.SubmissionSucceeded,
        1
      )

      val otherMessage = MovementMessageWithStatus(
        MessageId(2),
        LocalDateTime.now(),
        Some(LocalDateTime.now()),
        MessageType.GoodsReleased,
        node,
        MessageStatus.SubmissionSucceeded,
        2
      )

      "must get the appropriate messages when they exist and has the right channel type" in {

        val arrival = arbitrary[Arrival].sample.value copy (messages = NonEmptyList[MovementMessage](message, List.empty))

        repository.insert(arrival).futureValue

        // We copy the message node because the returned node isn't equal, even though it's
        // identical for our purposes. As it's not what we are really testing, we just copy the
        // original message across so it doesn't fail the equality check
        val result = repository
          .getMessagesOfType(arrival.arrivalId, arrival.channel, List(mType))
          .map(
            opt =>
              opt.map(
                ar =>
                  ar.messages
                    .asInstanceOf[List[MovementMessageWithStatus]]
                    .map(
                      x => x copy (message = node)
                    )
              )
          )
          .futureValue

        result mustBe defined
        result.get must contain theSameElementsAs List(message)

      }

      "must only return the appropriate messages when an arrival is matched" in {

        val arrival = arbitrary[Arrival].sample.value copy (messages = NonEmptyList[MovementMessage](message, List(otherMessage)))

        repository.insert(arrival).futureValue

        // As in the previous test.
        val result = repository
          .getMessagesOfType(arrival.arrivalId, arrival.channel, List(mType))
          .map(
            opt =>
              opt.map(
                ar =>
                  ar.messages
                    .asInstanceOf[List[MovementMessageWithStatus]]
                    .map(
                      x => x copy (message = node)
                    )
              )
          )
          .futureValue

        result mustBe defined
        result.get must contain theSameElementsAs List(message)

      }

      "must return an empty list when an arrival exists but no messages match" in {
        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), messages = NonEmptyList[MovementMessage](otherMessage, List.empty))

        repository.insert(arrival).futureValue
        val result = repository.getMessagesOfType(ArrivalId(1), arrival.channel, List(mType)).futureValue

        result mustBe defined
        result.get.messages mustEqual List()
      }

      "must return None when an arrival does not exist" in {
        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        repository.insert(arrival).futureValue
        val result = repository.getMessagesOfType(ArrivalId(2), arrival.channel, List(mType)).futureValue

        result must not be defined
      }

      "must return an empty list when an arrival exists but without any of the message type" in {
        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        repository.insert(arrival).futureValue
        val result = repository.getMessagesOfType(ArrivalId(2), arrival.channel, List(mType)).futureValue

        result mustBe empty
      }

      "must return None when an arrival does exist but with the wrong channel type" in {

        val arrival = arbitrary[Arrival].sample.value copy (channel = api)

        repository.insert(arrival).futureValue
        val result = repository.getMessagesOfType(arrival.arrivalId, web, List(mType)).futureValue

        result must not be defined

      }

    }

    "getWithoutMessages(arrivalId: ArrivalId)" - {
      "must get an arrival when it exists" in {

        val arrival                = arbitrary[Arrival].sample.value
        val arrivalWithoutMessages = ArrivalWithoutMessages.fromArrival(arrival)
        repository.insert(arrival).futureValue
        val result = repository.getWithoutMessages(arrival.arrivalId)

        whenReady(result) {
          r =>
            r.value mustEqual arrivalWithoutMessages
        }

      }

      "must return None when an arrival does not exist" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1))

        repository.insert(arrival).futureValue
        val result = repository.getWithoutMessages(ArrivalId(2), web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }

      }

      "must return None when a arrival exists, but with a different channel type" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), api)

        repository.insert(arrival).futureValue
        val result = repository.get(ArrivalId(1), web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }

      }
    }

    "getWithoutMessages(arrivalId: ArrivalId, channelFilter: ChannelType)" - {
      "must get an arrival when it exists and has the right channel type" in {

        val arrival                = arbitrary[Arrival].sample.value.copy(channel = api)
        val arrivalWithoutMessages = ArrivalWithoutMessages.fromArrival(arrival)
        repository.insert(arrival).futureValue
        val result = repository.getWithoutMessages(arrival.arrivalId, arrival.channel)

        whenReady(result) {
          r =>
            r.value mustEqual arrivalWithoutMessages
        }

      }

      "must return None when an arrival does not exist" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), channel = api)

        repository.insert(arrival).futureValue
        val result = repository.getWithoutMessages(ArrivalId(2), web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }

      }

      "must return None when an arrival exists, but with a different channel type" in {

        val arrival = arbitrary[Arrival].sample.value copy (arrivalId = ArrivalId(1), api)

        repository.insert(arrival).futureValue
        val result = repository.get(ArrivalId(1), web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }

      }
    }

    "fetchAllArrivals" - {

      val now               = LocalDateTime.now(clock)
      val oneMinuteLater    = now.withMinute(1)
      val twoMinutesLater   = now.withMinute(2)
      val threeMinutesLater = now.withMinute(3)
      val fourMinutesLater  = now.withMinute(4)

      "must return Arrival Movements that match an eoriNumber and channel type" in {

        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = eoriNumber, channel = api, lastUpdated = oneMinuteLater)
        val arrivalMovement2 =
          arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(arrivalId = ArrivalId(1), channel = api, lastUpdated = twoMinutesLater)
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(eoriNumber = eoriNumber, arrivalId = ArrivalId(2), channel = web, lastUpdated = threeMinutesLater)
        val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)

        await(repository.bulkInsert(allMovements))

        val expectedApiFetchAll1 = ResponseArrivals(
          Seq(
            ResponseArrival(
              arrivalMovement1.arrivalId,
              routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
              routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
              arrivalMovement1.movementReferenceNumber,
              arrivalMovement1.status,
              arrivalMovement1.created,
              arrivalMovement1.lastUpdated
            )
          ),
          1,
          1,
          1
        )

        val expectedApiFetchAll3 = ResponseArrivals(
          Seq(
            ResponseArrival(
              arrivalMovement3.arrivalId,
              routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
              routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
              arrivalMovement3.movementReferenceNumber,
              arrivalMovement3.status,
              arrivalMovement3.created,
              arrivalMovement3.lastUpdated
            )
          ),
          1,
          1,
          1
        )

        await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), api, None)) mustBe expectedApiFetchAll1
        await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None)) mustBe expectedApiFetchAll3
      }

      "must return Arrival Movements with eoriNumber that match legacy TURN and channel type" in {

        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = turn, channel = api, lastUpdated = oneMinuteLater)
        val arrivalMovement2 =
          arbitrary[Arrival].suchThat(_.eoriNumber != turn).sample.value.copy(arrivalId = ArrivalId(1), channel = api, lastUpdated = twoMinutesLater)
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2), eoriNumber = turn, channel = web, lastUpdated = threeMinutesLater)

        val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)
        await(repository.bulkInsert(allMovements))

        val expectedApiFetchAll1 = ResponseArrivals(
          Seq(
            ResponseArrival(
              arrivalMovement1.arrivalId,
              routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
              routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
              arrivalMovement1.movementReferenceNumber,
              arrivalMovement1.status,
              arrivalMovement1.created,
              arrivalMovement1.lastUpdated
            )
          ),
          1,
          1,
          1
        )

        val expectedApiFetchAll3 = ResponseArrivals(
          Seq(
            ResponseArrival(
              arrivalMovement3.arrivalId,
              routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
              routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
              arrivalMovement3.movementReferenceNumber,
              arrivalMovement3.status,
              arrivalMovement3.created,
              arrivalMovement3.lastUpdated
            )
          ),
          1,
          1,
          1
        )

        await(repository.fetchAllArrivals(Ior.left(TURN(turn)), api, None)) mustBe expectedApiFetchAll1
        await(repository.fetchAllArrivals(Ior.left(TURN(turn)), web, None)) mustBe expectedApiFetchAll3

      }

      "must return Arrival Movements with eoriNumber that match either EORI or legacy TURN and channel type" in {

        val ids: Set[String] = Set(eoriNumber, turn)

        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(0), eoriNumber = eoriNumber, channel = api, lastUpdated = oneMinuteLater)
        val arrivalMovement2 = arbitrary[Arrival]
          .suchThat(
            arrival => !ids.contains(arrival.eoriNumber)
          )
          .sample
          .value
          .copy(arrivalId = ArrivalId(1), channel = api, lastUpdated = twoMinutesLater)
        val arrivalMovement3 = arbitrary[Arrival].sample.value.copy(arrivalId = ArrivalId(2), eoriNumber = turn, channel = web, lastUpdated = threeMinutesLater)

        val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3)
        await(repository.bulkInsert(allMovements))

        val expectedApiFetchAll1 = ResponseArrivals(
          Seq(
            ResponseArrival(
              arrivalMovement1.arrivalId,
              routes.MovementsController.getArrival(arrivalMovement1.arrivalId).url,
              routes.MessagesController.getMessages(arrivalMovement1.arrivalId).url,
              arrivalMovement1.movementReferenceNumber,
              arrivalMovement1.status,
              arrivalMovement1.created,
              arrivalMovement1.lastUpdated
            )
          ),
          1,
          1,
          1
        )

        val expectedApiFetchAll3 = ResponseArrivals(
          Seq(
            ResponseArrival(
              arrivalMovement3.arrivalId,
              routes.MovementsController.getArrival(arrivalMovement3.arrivalId).url,
              routes.MessagesController.getMessages(arrivalMovement3.arrivalId).url,
              arrivalMovement3.movementReferenceNumber,
              arrivalMovement3.status,
              arrivalMovement3.created,
              arrivalMovement3.lastUpdated
            )
          ),
          1,
          1,
          1
        )

        await(repository.fetchAllArrivals(Ior.both(TURN(turn), EORINumber(eoriNumber)), api, None)) mustBe expectedApiFetchAll1
        await(repository.fetchAllArrivals(Ior.both(TURN(turn), EORINumber(eoriNumber)), web, None)) mustBe expectedApiFetchAll3

      }

      "must return an empty sequence when there are no movements with the same eori" in {

        val arrivalMovement1 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val arrivalMovement2 = arbitrary[Arrival].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)

        val allMovements = Seq(arrivalMovement1, arrivalMovement2)
        await(repository.bulkInsert(allMovements))

        val result = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), api, None))

        result mustBe ResponseArrivals(Seq.empty, 0, 0, 0)

      }

      "must filter results by lastUpdated when updatedSince parameter is provided" in {

        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(0),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31)
          )
        val arrivalMovement2 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(1),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 35, 32)
          )
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(2),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 12, 30, 21)
          )
        val arrivalMovement4 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(3),
            eoriNumber = eoriNumber,
            channel = web,
            lastUpdated = LocalDateTime.of(2021, 4, 30, 10, 15, 16)
          )

        val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)
        await(repository.bulkInsert(allMovements))

        // We must use the web channel for this test as the API max rows returned in integration test config is 2
//        val dateTime = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
        val dateTime = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 9, 30, 32), ZoneOffset.ofHours(1)) // Offset not registering?
        val actual   = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, Some(dateTime)))
        val expected = ResponseArrivals(Seq(arrivalMovement3, arrivalMovement4, arrivalMovement2).map(ResponseArrival.build), 3, 4, 3)

        actual mustEqual expected
      }

      "must filter results by mrn when mrn search parameter provided matches" in {
        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(30),
            eoriNumber = eoriNumber,
            channel = web,
            movementReferenceNumber = mrn,
            lastUpdated = oneMinuteLater
          )
        val arrivalMovement2 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(31),
            eoriNumber = eoriNumber,
            channel = web,
            movementReferenceNumber = mrn,
            lastUpdated = twoMinutesLater
          )
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(32),
            eoriNumber = eoriNumber,
            channel = web,
            movementReferenceNumber = mrn,
            lastUpdated = threeMinutesLater
          )
        val arrivalMovement4 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(33),
            eoriNumber = eoriNumber,
            channel = web,
            movementReferenceNumber = mrn,
            lastUpdated = fourMinutesLater
          )

        val allMovements = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)
        await(repository.bulkInsert(allMovements))

        val expectedAllMovements = allMovements.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse

        val actual   = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value)))
        val expected = ResponseArrivals(expectedAllMovements, 4, 4, 4)

        actual mustEqual expected
      }

      "must filter results by mrn when substring of a mrn search parameter provided matches" in {
        val arrivalMovement1 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(0),
            eoriNumber = eoriNumber,
            channel = web,
            movementReferenceNumber = mrn,
            lastUpdated = oneMinuteLater
          )
        val arrivalMovement2 =
          arbitrary[Arrival]
            .suchThat(_.movementReferenceNumber != mrn)
            .sample
            .value
            .copy(arrivalId = ArrivalId(1), eoriNumber = eoriNumber, channel = web, lastUpdated = twoMinutesLater)
        val arrivalMovement3 =
          arbitrary[Arrival].sample.value.copy(
            arrivalId = ArrivalId(2),
            eoriNumber = eoriNumber,
            channel = web,
            movementReferenceNumber = mrn,
            lastUpdated = threeMinutesLater
          )
        val arrivalMovement4 =
          arbitrary[Arrival]
            .suchThat(_.movementReferenceNumber != mrn)
            .sample
            .value
            .copy(arrivalId = ArrivalId(3), eoriNumber = eoriNumber, channel = web, lastUpdated = fourMinutesLater)

        val allMovements        = Seq(arrivalMovement1, arrivalMovement2, arrivalMovement3, arrivalMovement4)
        val allMovementsMatched = Seq(arrivalMovement1, arrivalMovement3)

        await(repository.bulkInsert(allMovements))
        val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse

        val actual = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value.substring(3, 9))))

        val expected = ResponseArrivals(expectedAllMovements, 2, 4, 2)

        actual mustEqual expected
      }

      "must filter results by mrn when mrn search parameter provided matches return match count" in {

        val arrivals = nonEmptyListOfNArrivals(10)
          .map(_.toList)
          .sample
          .value
          .map(
            arr => arr.copy(eoriNumber = eoriNumber, movementReferenceNumber = mrn, channel = web, lastUpdated = now.withSecond(arr.arrivalId.index))
          )

        val arrivalMovement1 =
          arbitrary[Arrival]
            .retryUntil(
              x => x.movementReferenceNumber != mrn && !arrivals.map(_.arrivalId).contains(x.arrivalId)
            )
            .sample
            .value
            .copy(eoriNumber = eoriNumber, channel = web, lastUpdated = now)

        val allArrivals = arrivalMovement1 :: arrivals

        val allMovementsMatched = arrivals

        val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse

        await(repository.bulkInsert(allArrivals))

        val actual = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value.substring(4, 9))))

        val expected = ResponseArrivals(expectedAllMovements, arrivals.size, allArrivals.size, arrivals.size)

        actual mustEqual expected

      }

      "must filter results by mrn when mrn search parameter  with case insensitive provided matches return match count" in {
        val arrivals = nonEmptyListOfNArrivals(3)
          .map(_.toList)
          .sample
          .value
          .map(
            arrival =>
              arrival.copy(eoriNumber = eoriNumber, movementReferenceNumber = mrn, channel = web, lastUpdated = now.withSecond(arrival.arrivalId.index))
          )

        val arrivalMovement1 =
          arbitrary[Arrival]
            .retryUntil(
              x => x.movementReferenceNumber != mrn && !arrivals.map(_.arrivalId).contains(x.arrivalId)
            )
            .sample
            .value
            .copy(eoriNumber = eoriNumber, channel = web, lastUpdated = now)

        val allArrivals = arrivalMovement1 :: arrivals

        await(repository.bulkInsert(allArrivals))

        val allMovementsMatched = arrivals

        val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse

        val actual = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, Some(mrn.value.substring(4, 9).toLowerCase())))

        val expected = ResponseArrivals(expectedAllMovements, arrivals.size, allArrivals.size, arrivals.size)

        actual mustEqual expected
      }

      "must return no results when an attempt at a regex is provided to the mrn search parameter" in {

        val arrival1 = arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(1),
          eoriNumber = eoriNumber,
          channel = ChannelType.web,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31),
          movementReferenceNumber = mrn
        )

        val arrival2 = arbitrary[Arrival]
          .suchThat(_.movementReferenceNumber != mrn)
          .sample
          .value
          .copy(
            arrivalId = ArrivalId(2),
            eoriNumber = eoriNumber,
            channel = ChannelType.web,
            lastUpdated = LocalDateTime.of(2021, 5, 30, 9, 35, 32)
          )

        val arrival3 = arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(3),
          eoriNumber = eoriNumber,
          channel = ChannelType.web,
          lastUpdated = LocalDateTime.of(2021, 6, 30, 9, 30, 21),
          movementReferenceNumber = mrn
        )
        val arrival4 = arbitrary[Arrival]
          .suchThat(_.movementReferenceNumber != mrn)
          .sample
          .value
          .copy(
            arrivalId = ArrivalId(4),
            eoriNumber = eoriNumber,
            channel = ChannelType.web,
            lastUpdated = LocalDateTime.of(2021, 7, 30, 10, 15, 16)
          )

        val allMovements = Seq(arrival1, arrival2, arrival3, arrival4)
        await(repository.bulkInsert(allMovements))

        val arrivals = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), ChannelType.web, None, Some(s"+$mrn"), Some(5)))

        arrivals mustBe ResponseArrivals(
          Seq(),
          0,
          4,
          0
        )

      }

      "must return no results when an invalid regex is provided" in {

        val arrival1 = arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(1),
          eoriNumber = eoriNumber,
          channel = ChannelType.web,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31),
          movementReferenceNumber = mrn
        )

        val arrival2 = arbitrary[Arrival]
          .suchThat(_.movementReferenceNumber != mrn)
          .sample
          .value
          .copy(
            arrivalId = ArrivalId(2),
            eoriNumber = eoriNumber,
            channel = ChannelType.web,
            lastUpdated = LocalDateTime.of(2021, 5, 30, 9, 35, 32)
          )

        val arrival3 = arbitrary[Arrival].sample.value.copy(
          arrivalId = ArrivalId(3),
          eoriNumber = eoriNumber,
          channel = ChannelType.web,
          lastUpdated = LocalDateTime.of(2021, 6, 30, 9, 30, 21),
          movementReferenceNumber = mrn
        )
        val arrival4 = arbitrary[Arrival]
          .suchThat(_.movementReferenceNumber != mrn)
          .sample
          .value
          .copy(
            arrivalId = ArrivalId(4),
            eoriNumber = eoriNumber,
            channel = ChannelType.web,
            lastUpdated = LocalDateTime.of(2021, 7, 30, 10, 15, 16)
          )

        val allMovements = Seq(arrival1, arrival2, arrival3, arrival4)
        await(repository.bulkInsert(allMovements))

        val arrivals =
          await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), ChannelType.web, None, Some("a.+"), Some(5)))

        arrivals mustBe ResponseArrivals(
          Seq(),
          0,
          4,
          0
        )

      }

      "must fetch all results based on pageSize 5 for page number 2" in {
        val arrivals = nonEmptyListOfNArrivals(20)
          .map(_.toList)
          .sample
          .value
          .map(
            arrival =>
              arrival.copy(eoriNumber = eoriNumber, movementReferenceNumber = mrn, channel = web, lastUpdated = now.withSecond(arrival.arrivalId.index))
          )

        val pageSize    = 5
        val page        = 2
        val allArrivals = arrivals

        await(repository.bulkInsert(allArrivals))

        val allMovementsMatched  = arrivals
        val expectedAllMovements = allMovementsMatched.map(ResponseArrival.build).sortBy(_.updated)(_ compareTo _).reverse.slice(5, 10)

        val actual = await(repository.fetchAllArrivals(Ior.right(EORINumber(eoriNumber)), web, None, None, Some(pageSize), Some(page)))

        val expected = ResponseArrivals(expectedAllMovements, pageSize, allArrivals.size, allArrivals.size)

        actual mustEqual expected

      }
    }

    "getMessage" - {
      "must return Some(message) if arrival and message exists" in {

        val message  = arbitrary[models.MovementMessageWithStatus].sample.value.copy(messageId = MessageId(1))
        val messages = new NonEmptyList(message, Nil)
        val arrival  = arbitrary[Arrival].sample.value.copy(channel = api, messages = messages)

        await(repository.insert(arrival))
        val result = repository.getMessage(arrival.arrivalId, arrival.channel, MessageId(1))

        whenReady(result) {
          r =>
            r.isDefined mustBe true
            r.value mustEqual message
        }
      }

      "must return None if departure does not exist" in {
        val result = repository.getMessage(ArrivalId(1), api, MessageId(1))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None if message does not exist" in {

        val message  = arbitrary[models.MovementMessageWithStatus].sample.value.copy(messageId = MessageId(1))
        val messages = new NonEmptyList(message, Nil)
        val arrival  = arbitrary[Arrival].sample.value.copy(channel = api, messages = messages)

        await(repository.insert(arrival))
        val result = repository.getMessage(arrival.arrivalId, arrival.channel, MessageId(5))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }

      }
    }
  }
}
