package services.repositories

import models.request.InternalReferenceId
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import repositories.InternalReferenceIdRepository

import scala.concurrent.ExecutionContext.Implicits.global

class InternalReferenceIdRepositorySpec extends FreeSpec with MustMatchers with ScalaFutures with MongoSuite with GuiceOneAppPerSuite with IntegrationPatience {

  val service =  app.injector.instanceOf[InternalReferenceIdRepository]

  "InternalReferenceIdRepository" - {

    "must generate InternalReferenceId when no record exists within the database" in {

      database.flatMap(_.drop()).futureValue

      val first = service.nextId().futureValue

      first mustBe InternalReferenceId(1)

      val second = service.nextId().futureValue

      second mustBe InternalReferenceId(2)
    }
  }
}
