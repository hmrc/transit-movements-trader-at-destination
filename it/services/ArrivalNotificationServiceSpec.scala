package it.services

import generators.MessageGenerators
import models.messages.{ArrivalNotification, NormalNotification, SimplifiedNotification}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsObject, Json}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import repositories.CollectionNames
import services.ArrivalNotificationService
import services.mocks.MockDateTimeService

import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalNotificationServiceSpec
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

  val service: ArrivalNotificationService = app.injector.instanceOf[ArrivalNotificationService]

  "ArrivalNotificationService" - {

    "must persist NormalNotification within mongoDB" in {

      forAll(arbitrary[NormalNotification]) {

        normalNotification =>

          database.flatMap(_.drop()).futureValue

          service.persistToMongo(normalNotification).futureValue

          val selector = Json.obj("movementReferenceNumber" -> normalNotification.movementReferenceNumber)

          val getValue: Option[ArrivalNotification] = database.flatMap {
            result =>
              result.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
                .find(selector, None)
                .one[ArrivalNotification]
          }.futureValue

          getValue.value mustBe normalNotification
      }
    }

    "must persist SimplifiedNotification within mongoDB" in {

      forAll(arbitrary[SimplifiedNotification]) {

        simplifiedNotification =>

          database.flatMap(_.drop()).futureValue

          service.persistToMongo(simplifiedNotification).futureValue

          val selector = Json.obj("movementReferenceNumber" -> simplifiedNotification.movementReferenceNumber)

          val getValue: Option[ArrivalNotification] = database.flatMap {
            result =>
              result.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
                .find(selector, None)
                .one[ArrivalNotification]
          }.futureValue

          getValue.value mustBe simplifiedNotification
      }
    }

    "must delete NormalNotification from MongoDB" in {

      forAll(arbitrary[NormalNotification]) {

        normalNotification =>

          val json: JsObject = Json.toJsObject(normalNotification)

          database.flatMap {
            db =>
              db.drop().flatMap {
                _ =>
                  db.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
                    .insert(false)
                    .one(json)
              }
          }.futureValue

          service.deleteFromMongo(normalNotification.movementReferenceNumber).futureValue

          val result = database.flatMap(_.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
            .find(Json.obj("movementReferenceNumber" -> normalNotification.movementReferenceNumber), None)
            .one[NormalNotification]).futureValue

          result mustBe None
      }
    }

    "must delete SimplifiedNotification from MongoDB" in {

      forAll(arbitrary[SimplifiedNotification]) {

        simplifiedNotification =>

          database.flatMap(_.drop()).futureValue

          val json: JsObject = Json.toJsObject(simplifiedNotification)

          database.flatMap {
            db =>
              db.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
                .insert(false)
                .one(json)
          }.futureValue

          service.deleteFromMongo(simplifiedNotification.movementReferenceNumber).futureValue

          val result = database.flatMap(_.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
            .find(Json.obj("movementReferenceNumber" -> simplifiedNotification.movementReferenceNumber), None)
            .one[SimplifiedNotification]).futureValue

          result mustBe None
      }
    }
  }

}
