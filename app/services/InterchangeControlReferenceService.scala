/*
 * Copyright 2019 HM Revenue & Customs
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

package services

import javax.inject.Inject
import models.messages.request.InterchangeControlReference
import repositories.SequentialInterchangeControlReferenceIdRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait WriteFailure

object FailedCreatingInterchangeControlReference extends WriteFailure

class InterchangeControlReferenceServiceImpl @Inject()(sequentialInterchangeControlReferenceIdRepository: SequentialInterchangeControlReferenceIdRepository) extends InterchangeControlReferenceService {

  def getInterchangeControlReferenceId: Future[Either[WriteFailure, InterchangeControlReference]] = {

    sequentialInterchangeControlReferenceIdRepository.nextInterchangeControlReferenceId().map {
      reference => Right(reference)
    }.recover {
      case _ =>
        Left(FailedCreatingInterchangeControlReference)
    }
  }

}

trait InterchangeControlReferenceService {
  def getInterchangeControlReferenceId: Future[Either[WriteFailure, InterchangeControlReference]]
}

