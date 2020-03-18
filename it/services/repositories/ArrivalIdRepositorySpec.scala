package services.repositories

import models.request.ArrivalId
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import reactivemongo.play.json.collection.JSONCollection
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import repositories.ArrivalIdRepository

import scala.concurrent.ExecutionContext.Implicits.global

class ArrivalIdRepositorySpec extends FreeSpec with MustMatchers with ScalaFutures with MongoSuite with GuiceOneAppPerSuite with IntegrationPatience {

  private val service =  app.injector.instanceOf[ArrivalIdRepository]

  "ArrivalIdRepository" - {

    "must generate sequential ArrivalIds starting at 1 when no record exists within the database" in {

      database.flatMap(_.drop()).futureValue

      val first = service.nextId().futureValue
      val second = service.nextId().futureValue

      first mustBe ArrivalId(1)
      second mustBe ArrivalId(2)
    }

    "must generate sequential ArrivalIds when a record exists within the database" in {

      database.flatMap {
        db =>
          db.drop().flatMap {
            _ =>
              db.collection[JSONCollection](ArrivalIdRepository.collectionName)
                .insert(ordered = false)
                .one(
                  Json.obj(
                    "_id" -> "record_id",
                    "last-index" -> 1
                  ))
          }
      }.futureValue

      val first = service.nextId().futureValue
      val second = service.nextId().futureValue

      first mustBe ArrivalId(2)
      second mustBe ArrivalId(3)
    }
  }
}
