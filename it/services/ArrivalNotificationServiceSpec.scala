package services

import java.time.LocalDate

import it.services.MongoSuite
import models.TraderWithoutEori
import models.messages.{ArrivalNotification, NormalNotification}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import repositories.CollectionNames
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
    with OptionValues {

  val service: ArrivalNotificationService = app.injector.instanceOf[ArrivalNotificationService]

  "ArrivalNotificationService" - {
    "must persist ArrivalNotification within mongoDB" in {

      val arrivalNotification: ArrivalNotification =
        NormalNotification(
          "test",
          "test",
          LocalDate.now(),
          Some(""),
          TraderWithoutEori.apply("name", "street", "postCode", "city", "countryCode"),
          "test",
          Nil
        )

      database.flatMap(_.drop()).futureValue
      service.persistToMongo(arrivalNotification).futureValue

      val selector = Json.obj("movementReferenceNumber" -> "test")

      val getValue: Option[ArrivalNotification] = database.flatMap {
        result =>
          result.collection[JSONCollection](CollectionNames.ArrivalNotificationCollection)
            .find(selector, None)
            .one[ArrivalNotification]
      }.futureValue

      getValue.value mustBe arrivalNotification
    }
  }
}
