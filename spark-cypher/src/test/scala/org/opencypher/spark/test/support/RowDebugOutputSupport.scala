/*
 * Copyright (c) 2016-2018 "Neo4j, Inc." [https://neo4j.com]
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
package org.opencypher.okapi.test.support

import org.apache.spark.sql.Row
import org.opencypher.okapi.ir.test.support.DebugOutputSupport

import scala.collection.Bag

trait RowDebugOutputSupport extends DebugOutputSupport {

  implicit class RowPrinter(bag: Bag[Row]) {
    def debug(): String = {
      val rowStrings = bag.map { row =>
        val rowAsString = row.toSeq.map {
          case null => "null"
          case s: String => s""""$s""""
          case l: Long => s"""${l}L"""
          case other => other.toString
        }

        rowAsString.mkString("Row(", ", ", ")")
      }

      rowStrings.mkString("Bag(", ",\n", ")")
    }

    def printRows(): Unit = println(debug())
  }

  implicit class IterableToBagConverter(val elements: Iterable[Row]) {
    def toBag: Bag[Row] = Bag(elements.toSeq: _*)
  }

  implicit class ArrayToBagConverter(val elements: Array[Row]) {
    def toBag: Bag[Row] = Bag(elements.toSeq: _*)
  }

}
