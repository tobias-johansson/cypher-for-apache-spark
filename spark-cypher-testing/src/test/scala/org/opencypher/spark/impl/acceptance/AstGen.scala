package org.opencypher.spark.impl.acceptance

import org.opencypher.okapi.api.value.CypherValue.CypherMap
import org.opencypher.okapi.relational.api.graph.RelationalCypherGraph
import org.opencypher.okapi.relational.api.planning.RelationalCypherResult
import org.opencypher.okapi.testing.Bag
import org.opencypher.okapi.testing.Bag.Bag
import org.opencypher.spark.impl.table.SparkTable.DataFrameTable
import org.opencypher.spark.testing.CAPSTestSuite
import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.{expressions => exp}
import org.opencypher.v9_0.util.InputPosition
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks

import scala.collection.JavaConverters._

class AstGen extends CAPSTestSuite with ScanGraphInit with PropertyChecks {

  val p = InputPosition(0, 0, 0)

  def _call(name: Gen[String], arg: Gen[exp.Expression]) = for {
    name <- name
    arg <- arg
  } yield exp.FunctionInvocation(exp.Namespace()(p), exp.FunctionName(name)(p), false, IndexedSeq(arg))(p)

  implicit class ExprOps(expr: Gen[exp.Expression]) {
    def as(as: String) = for {
      expr <- expr
      item = AliasedReturnItem(expr, exp.Variable(as)(p))(p)
    } yield item
  }

  def _var(name: String) =
    const(exp.Variable(name)(p))

  def _return(item: Gen[ReturnItem]) = for {
    item <- item
    res = Return(ReturnItems(false, List(item))(p))(p)
  } yield res

  def _with(item: Gen[ReturnItem]) = for {
    item <- item
    res = With(ReturnItems(false, List(item))(p))(p)
  } yield res

  def _match() =
    Match(false, exp.Pattern(Seq(exp.NamedPatternPart)))

  def _query(clauses: Gen[Clause]*) = for {
    clauses <- Gen.sequence(clauses)
    res = Query(None, SingleQuery(clauses.asScala)(p))(p)
  } yield res

  implicit class GraphOps(graph: RelationalCypherGraph[DataFrameTable]) {
    def exec(stmt: Statement): RelationalCypherResult[DataFrameTable] =
      caps.cypherOnGraph(graph, "", stmt, CypherMap.empty, SemanticState.clean, Set(), Map(), None)
  }

  implicit class ResultOps(result: RelationalCypherResult[DataFrameTable]) {
    def collected: Bag[CypherMap] =
      result.records.toMapsWithCollectedEntities
  }

  val _unaryFunction = Gen.oneOf(
    "id", "date", "min", "max"
  )

  val _null = const(exp.Null()(p))

  it("run") {

    val graph = initGraph("""CREATE ()""")

    forAll(
      _query(_return(_call(_unaryFunction, _null).as("res")))
    ) { q =>
      graph.exec(q).collected shouldBe Bag(
        CypherMap("res" -> null)
      )
    }


  }

}
/*
Query(None,
  SingleQuery(List(With(false,ReturnItems(false,List(AliasedReturnItem(FunctionInvocation(Namespace(List()),FunctionName(id),false,Vector(Null())),Variable(  FRESHID7)))),None,None,None,None), Return(false,ReturnItems(false,List(AliasedReturnItem(Variable(  FRESHID7),Variable(res)))),None,None,None,Set()))))
*/
