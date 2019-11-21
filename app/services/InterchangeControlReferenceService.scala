package services

import javax.inject.Inject
import repositories.SequentialInterchangeControlReferenceIdRepository
import scala.concurrent.ExecutionContext.Implicits.global

class InterchangeControlReferenceService @Inject()(sequentialInterchangeControlReferenceIdRepository: SequentialInterchangeControlReferenceIdRepository) {

  def getInterchangeControlReferenceId: Unit = {

    sequentialInterchangeControlReferenceIdRepository.nextInterchangeControlReferenceId().recover{
      case e: InternalError =>
    }

  }

}
