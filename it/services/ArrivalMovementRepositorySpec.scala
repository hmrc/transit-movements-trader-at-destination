package services

import generators.MessageGenerators
import it.services.MongoSuite
import models.ArrivalMovement
import models.messages.{ArrivalNotificationMessage, MovementReferenceNumber}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsObject, Json}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import repositories.{ArrivalMovementRepository, CollectionNames}
import services.mocks.MockDateTimeService

import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalMovementRepositorySpec
  extends FreeSpec
    with MustMatchers
    with MongoSuite
    with ScalaFutures
    with GuiceOneAppPerSuite
    with IntegrationPatience
    with MockDateTimeService
    with OptionValues
    with ScalaCheckPropertyChecks
    with MessageGenerators {

  private val service: ArrivalMovementRepository = app.injector.instanceOf[ArrivalMovementRepository]

  "ArrivalMovementRepository" - {

    "must persist ArrivalMovement within mongoDB" in {

      forAll(arbitrary[ArrivalMovement]) {
        arrivalMovement =>

          database.flatMap(_.drop()).futureValue

          service.persistToMongo(arrivalMovement).futureValue

          val selector = Json.obj("movementReferenceNumber" -> arrivalMovement.movementReferenceNumber)

          val getValue: Option[ArrivalMovement] = database.flatMap {
            result =>
              result.collection[JSONCollection](CollectionNames.ArrivalMovementCollection)
                .find(selector, None)
                .one[ArrivalMovement]
          }.futureValue

          getValue.value mustBe arrivalMovement
      }
    }


    "must delete NormalNotification from MongoDB" in {

      forAll(arbitrary[ArrivalMovement]) {
        arrivalMovement =>

          database.flatMap(_.drop()).futureValue

          val json: JsObject = Json.toJsObject(arrivalMovement)

          database.flatMap {
            db =>
              db.collection[JSONCollection](CollectionNames.ArrivalMovementCollection)
                .insert(false)
                .one(json)
          }.futureValue

          val selector = Json.obj("movementReferenceNumber" -> arrivalMovement.movementReferenceNumber)

          service.deleteFromMongo(arrivalMovement.movementReferenceNumber).futureValue

          val result = database.flatMap(_.collection[JSONCollection](CollectionNames.ArrivalMovementCollection)
            .find(selector, None)
            .one[ArrivalMovement]).futureValue

          result mustBe None
      }
    }

    "must fetch all the ArrivalMovements from mongoDB" in {

      forAll(seqWithMaxLength[ArrivalMovement](10), seqWithMaxLength[ArrivalMovement](10)) {
        (arrivalMovement, arrivalMovementWithDifferentEori) =>


        val arrivalMovementWithMatchingEori =  arrivalMovement.map(_.copy(eoriNumber = "AA11111"))

          val allMovements = arrivalMovementWithMatchingEori ++ arrivalMovementWithDifferentEori

          database.flatMap(_.drop()).futureValue

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](CollectionNames.ArrivalMovementCollection)
                .insert(false)
                .many(jsonArr)
          }.futureValue

          val result: Seq[ArrivalMovement] = service.fetchAllMovements("AA11111").futureValue

          result mustBe arrivalMovementWithMatchingEori
      }
    }

  }

}

