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

import play.api.libs.json.JsObject
import reactivemongo.api.ReadConcern
import reactivemongo.api.WriteConcern
import reactivemongo.api.commands.FindAndModifyCommand
import reactivemongo.play.json.JSONSerializationPack
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait Repository {

  implicit class JSONCollectionImplicits(coll: JSONCollection) {

    def simpleCount(
      selector: JsObject
    )(implicit ec: ExecutionContext): Future[Int] =
      coll
        .count(
          selector = Some(selector),
          limit = None,
          skip = 0,
          hint = None,
          readConcern = ReadConcern.Local
        )
        .map(_.toInt)

    def simpleFindAndUpdate(
      selector: JsObject,
      update: JsObject,
      fetchNewObject: Boolean = false,
      upsert: Boolean = false
    )(implicit ec: ExecutionContext): Future[FindAndModifyCommand.Result[JSONSerializationPack.type]] =
      coll
        .findAndUpdate(
          selector = selector,
          update = update,
          fetchNewObject = fetchNewObject,
          upsert = upsert,
          sort = None,
          fields = None,
          bypassDocumentValidation = false,
          writeConcern = WriteConcern.Default,
          maxTime = None,
          collation = None,
          arrayFilters = Nil
        )

    def simpleFindAndRemove(
      selector: JsObject
    )(implicit ec: ExecutionContext): Future[FindAndModifyCommand.Result[JSONSerializationPack.type]] =
      coll
        .findAndRemove(
          selector = selector,
          sort = None,
          fields = None,
          writeConcern = WriteConcern.Default,
          maxTime = None,
          collation = None,
          arrayFilters = Nil
        )
  }

}
