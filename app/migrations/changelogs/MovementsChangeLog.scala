/*
 * Copyright 2021 HM Revenue & Customs
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

package migrations.changelogs

import cats.syntax.all._
import com.github.cloudyrock.mongock.ChangeLog
import com.github.cloudyrock.mongock.ChangeSet
import com.mongodb.client.MongoDatabase
import com.mongodb.client.model._
import org.bson.Document
import repositories.ArrivalMovementRepository

import scala.collection.JavaConverters._

@ChangeLog(order = "001")
class MovementsChangeLog {

  @ChangeSet(order = "001", id = "addMessageIdToMessages", author = "transit-movements-trader-at-destination", runAlways = true)
  def addMessageIdToMessages(mongo: MongoDatabase): Unit = {
    val collection = mongo.getCollection(ArrivalMovementRepository.collectionName)

    collection
      .find()
      .asScala
      .foreach {
        doc =>
          val messages = doc
            .getList("messages", classOf[Document])
            .asScala
            .toList

          if (messages.nonEmpty) {
            collection.updateOne(
              Filters.eq("_id", doc.getInteger("_id")),
              Updates.combine(
                messages.mapWithIndex {
                  case (_, index) =>
                    Updates.set(s"messages.$index.messageId", index + 1)
                }.asJava
              )
            )
          }
      }
  }
}
