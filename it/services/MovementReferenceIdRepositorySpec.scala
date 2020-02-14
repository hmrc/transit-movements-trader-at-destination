package services
import it.services.MongoSuite
import models.request.MovementReferenceId
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import repositories.MovementReferenceIdRepository

import scala.concurrent.ExecutionContext.Implicits.global

class MovementReferenceIdRepositorySpec extends FreeSpec with MustMatchers with ScalaFutures with MongoSuite with GuiceOneAppPerSuite with IntegrationPatience {

  val service =  app.injector.instanceOf[MovementReferenceIdRepository]

  "MovementReferenceIdRepository" - {

    "must generate MovementReferenceId when no record exists within the database" in {

      database.flatMap(_.drop()).futureValue

      val first = service.nextId().futureValue

      first mustBe MovementReferenceId(1)

      val second = service.nextId().futureValue

      second mustBe MovementReferenceId(2)
    }
  }
}
