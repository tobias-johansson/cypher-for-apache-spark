package org.opencypher.spark.impl

import org.apache.spark.sql._
import org.opencypher.spark.api.types.CTNode
import org.opencypher.spark.api.{CypherResultContainer, PropertyGraph}
import org.opencypher.spark.api.value.{CypherNode, CypherRelationship, CypherValue}
import org.opencypher.spark.impl.frame._
import org.opencypher.spark.impl.prototype._
import org.opencypher.spark.impl.util.SlotSymbolGenerator

import scala.language.implicitConversions

    // TODO
    // (2) Benchmarking
    // val unboundedVarLength..
    // val shortestPath=...
    // val patternPredicates=...
    // val skipLimit=...
    // (3) Basic expression handling
    // (4) Slot-field relationship
    // (5) UDTs

class StdPropertyGraph(val nodes: Dataset[CypherNode], val relationships: Dataset[CypherRelationship])
                      (implicit val session: SparkSession) extends PropertyGraph {

  implicit val planningContext = new PlanningContext(new SlotSymbolGenerator, nodes, relationships)
  implicit val runtimeContext = new StdRuntimeContext(session, Map.empty)


  val frames = new FrameProducer
  import frames._

  override def cypher(query: SupportedQuery): CypherResultContainer = {
    implicit val pInner = planningContext


    query match {

      case NodeScan(labels) =>
        StdCypherResultContainer.fromProducts(labelScan('n)(labels).asProduct)

      case NodeScanWithProjection(labels, firstKey, secondKey) =>
        val productFrame = labelScan('n)(labels).asProduct
        val withName = productFrame.propertyValue('n, firstKey)(Symbol("n.name"))
        val withAge = withName.propertyValue('n, secondKey)(Symbol("n.age"))
        val selectFields = withAge.selectFields(Symbol("n.name"), Symbol("n.age"))

        StdCypherResultContainer.fromProducts(selectFields)

      case SimplePattern(startLabels, types, endLabels) if startLabels.isEmpty && endLabels.isEmpty =>
        val relationships = typeScan('r)(types).asRow

        StdCypherResultContainer.fromRows(relationships)

      case SimplePattern(startLabels, types, endLabels) =>
        val aAsProduct = labelScan('a)(startLabels).asProduct
        val aWithId = aAsProduct.nodeId('a)(Symbol("id(a)"))

        val bAsProduct = labelScan('b)(endLabels).asProduct
        val bWithId = bAsProduct.nodeId('b)(Symbol("id(b)"))

        val rAsProduct = typeScan('r)(types).asProduct
        val rWithStartId = rAsProduct.relationshipStartId('r)(Symbol("startId(r)"))
        val rWithStartAndEndId = rWithStartId.relationshipEndId('r)(Symbol("endId(r)"))
        val relsAsRows = rWithStartAndEndId.asRow

        val joinRelA = relsAsRows.join(aWithId.asRow).on(rWithStartId.projectedField.sym)(aWithId.projectedField.sym)
        val joinRelB = joinRelA.join(bWithId.asRow).on(rWithStartAndEndId.projectedField.sym)(bWithId.projectedField.sym)

        val selectField = joinRelB.asProduct.selectFields('r)

        StdCypherResultContainer.fromProducts(selectField)

      case SimplePatternIds(startLabels, types, endLabels) =>
        val aAsProduct = labelScan('a)(startLabels).asProduct
        val aWithId = aAsProduct.nodeId('a)('aid).dropField('a)

        val bAsProduct = labelScan('b)(endLabels).asProduct
        val bWithId = bAsProduct.nodeId('b)('bid).dropField('b)

        val rAsProduct = typeScan('r)(types).asProduct.relationshipId('r)('rid)
        val rWithStartId = rAsProduct.relationshipStartId('r)('rstart)
        val rWithStartAndEndId = rWithStartId.relationshipEndId('r)('rend).dropField('r)
        val relsAsRows = rWithStartAndEndId.asRow

        val joinRelA = relsAsRows.join(aWithId.asRow).on('rstart)('aid)
        val joinRelB = joinRelA.join(bWithId.asRow).on('rend)('bid)

        val selectField = joinRelB.asProduct.selectFields('rid)

        StdCypherResultContainer.fromProducts(selectField)

      case SimpleUnionAll(lhsLabels, lhsKey, rhsLabels, rhsKey) =>
        val aAsProduct = labelScan('a)(lhsLabels).asProduct
        val aNames = aAsProduct.propertyValue('a, lhsKey)(Symbol("a.name"))
        val aNameRenamed = aNames.aliasField(Symbol("a.name") -> 'name)
        val selectFieldA = aNameRenamed.selectFields('name)

        val bAsProduct = labelScan('b)(rhsLabels).asProduct
        val bNames = bAsProduct.propertyValue('b, rhsKey)(Symbol("b.name"))
        val bNameRenamed = bNames.aliasField(Symbol("b.name") -> 'name)
        val selectFieldB = bNameRenamed.selectFields('name)

        val union = selectFieldA.unionAll(selectFieldB)

        StdCypherResultContainer.fromProducts(union)

      case NodeScanIdsSorted(labels) =>
        val nodeWithId = labelScan('n)(labels).asProduct.nodeId('n)('nid).dropField('n)

        val sorted = nodeWithId.orderBy(SortItem('nid, Desc)).selectFields('nid).aliasField('nid -> 'id)

        StdCypherResultContainer.fromProducts(sorted)

      case CollectNodeProperties(labels, key) =>
        val nodesWithProperty = labelScan('a)(labels).asProduct.propertyValue('a, key)('name)

        val grouped = nodesWithProperty.groupBy()(Collect('name)('names))

        StdCypherResultContainer.fromProducts(grouped)

      case CollectAndUnwindNodeProperties(labels, key, column) =>
        val nodesWithProperty = labelScan('a)(labels).asProduct.propertyValue('a, key)('name)

        val grouped = nodesWithProperty.groupBy()(Collect('name)('names))
        val unwindedAndSelected = grouped.unwind('names, column).selectFields(column)

        StdCypherResultContainer.fromProducts(unwindedAndSelected)

      case MatchOptionalExpand(startLabels, types, endLabels) =>
        val aNodes = labelScan('a)(startLabels).asProduct.nodeId('a)('aid).asRow

        val bNodes = labelScan('b)(endLabels).asProduct.nodeId('b)('bid).asRow
        val rRels = typeScan('r)(types).asProduct
            .relationshipStartId('r)('rstart)
            .relationshipEndId('r)('rend).asRow

        val joinRB = bNodes.join(rRels).on('bid)('rend)
        val joinAR = aNodes.optionalJoin(joinRB).on('aid)('rstart)

        val selected = joinAR.asProduct.selectFields('r)

        StdCypherResultContainer.fromProducts(selected)

      case BoundVariableLength(startLabels, lowerBound, upperBound) =>
        val aNodes = labelScan('a)(startLabels).asProduct

        val expanded = aNodes.varExpand('a, lowerBound, upperBound)('r)

        val selected = expanded.selectFields('r)

        StdCypherResultContainer.fromProducts(selected)

//        val lhs = nodes.filter(_.labels.contains("A")).map(node => (node.id.v, node))(Encoders.tuple(implicitly[Encoder[Long]], CypherValue.implicits.cypherValueEncoder[CypherNode])).toDF("id_a", "val_a")
//
//        val b = nodes.filter(_.labels.contains("B")).map(node => (node.id.v, node))(Encoders.tuple(implicitly[Encoder[Long]], CypherValue.implicits.cypherValueEncoder[CypherNode])).toDF("id_b", "val_b")
//        val rels = relationships.map(rel => (rel.start.v, rel.end.v, rel.id.v, rel))(Encoders.tuple(implicitly[Encoder[Long]], implicitly[Encoder[Long]], implicitly[Encoder[Long]], CypherValue.implicits.cypherValueEncoder[CypherRelationship])).toDF("start_rel", "end_rel", "id_rel", "val_rel")
//        val rhs = rels.join(b, functions.expr("id_b = end_rel"))
//
//        val joined = lhs.join(rhs, functions.expr("id_a = start_rel"), "left_outer")
//
//        val rel = joined.select(new Column("val_rel").as("value"))
//        val result = rel.as[CypherRelationship](CypherValue.implicits.cypherValueEncoder[CypherRelationship])
//
//        new StdFrame(result.map(r => StdRecord(Array(r), Array.empty)), ListMap("r" -> 0)).result
//
//      case SupportedQueries.simpleUnionDistinct =>
//        val a = nodes.filter(_.labels.contains("A")).map(node => node.properties.getOrElse("name", CypherNull))(CypherValue.implicits.cypherValueEncoder[CypherValue]).toDF("name")
//        val b = nodes.filter(_.labels.contains("B")).map(node => node.properties.getOrElse("name", CypherNull))(CypherValue.implicits.cypherValueEncoder[CypherValue]).toDF("name")
//        val result = a.union(b).distinct().as[CypherValue](CypherValue.implicits.cypherValueEncoder[CypherValue])
//
//        new StdFrame(result.map(v => StdRecord(Array(v), Array.empty)), ListMap("name" -> 0)).result
//
//      case SupportedQueries.unwind =>
//        val l = CypherList(Seq(1, 2, 3).map(CypherInteger(_)))
//        val start = session.createDataset(Seq(l))(CypherValue.implicits.cypherValueEncoder[CypherList])
//
//        val result = start.flatMap(_.v)(CypherValue.implicits.cypherValueEncoder[CypherValue])
//
//        new StdFrame(result.map(v => StdRecord(Array(v), Array.empty)), ListMap("x" -> 0)).result
//
//
//      case SupportedQueries.shortestPath =>
//        val a = nodes.flatMap(_.properties.get("name").filter(_ == "Ava"))(CypherValue.implicits.cypherValueEncoder[CypherValue])
//        val b = nodes.flatMap(_.properties.get("name").filter(_ == "Sasha"))(CypherValue.implicits.cypherValueEncoder[CypherValue])
//
//        ???
//
//      case SupportedQueries.boundVarLength =>
//        val a = nodes.filter(_.labels.contains("A")).map(node => (node.id.v, node))(Encoders.tuple(implicitly[Encoder[Long]], CypherValue.implicits.cypherValueEncoder[CypherNode])).toDF("id_a", "val_a")
//        val b = nodes.filter(_.labels.contains("B")).map(node => (node.id.v, node))(Encoders.tuple(implicitly[Encoder[Long]], CypherValue.implicits.cypherValueEncoder[CypherNode])).toDF("id_b", "val_b")
//        val rels1 = relationships.map(rel => (rel.start.v, rel.end.v, rel.id.v, rel))(Encoders.tuple(implicitly[Encoder[Long]], implicitly[Encoder[Long]], implicitly[Encoder[Long]], CypherValue.implicits.cypherValueEncoder[CypherRelationship])).toDF(
//          "start_rel1", "end_rel1", "id_rel1", "val_rel1"
//        )
//        val rels2 = rels1.select(
//          new Column("start_rel1").as("start_rel2"),
//          new Column("end_rel1").as("end_rel2"),
//          new Column("id_rel1").as("id_rel2"),
//          new Column("val_rel1").as("val_rel2")
//        )
//
//        val step1out = rels1.join(a, functions.expr("id_a = start_rel1"))
//        val step1done = step1out.join(b, functions.expr("end_rel1 = id_b"))
//
//        val prepare1 = step1done.select(new Column("val_rel1").as("r"))
//        val result1 = prepare1
//          .as[CypherRelationship](CypherValue.implicits.cypherValueEncoder[CypherRelationship])
//          .map(r => CypherList(Seq(r)))(CypherValue.implicits.cypherValueEncoder[CypherList])
//
//        val step2out = step1out.join(rels2, functions.expr("end_rel1 = start_rel2"))
//        val step2done = step2out.join(b, functions.expr("end_rel2 = id_b"))
//
//        val prepare2 = step2done.select(new Column("val_rel1").as("r1"), new Column("val_rel2").as("r2"))
//        val encoder2 = ExpressionEncoder.tuple(Seq(CypherValue.implicits.cypherValueEncoder[CypherRelationship], CypherValue.implicits.cypherValueEncoder[CypherRelationship]).map(_.asInstanceOf[ExpressionEncoder[_]])).asInstanceOf[Encoder[Product]]
//        val result2 = prepare2
//          .as[Product](encoder2)
//          .map(p => CypherList(p.productIterator.map(_.asInstanceOf[CypherRelationship]).toList))(CypherValue.implicits.cypherValueEncoder[CypherList])
//
//        val result = result1.union(result2)
//
//        new StdFrame(result.map(r => StdRecord(Array(r), Array.empty)), ListMap("r" -> 0)).result
//
//      // *** Functionality left to test
//
//      // [X] Scans (via df access)
//      // [X] Projection (via ds.map)
//      // [X] Predicate filter (via ds.map to boolean and ds.filter)
//      // [X] Expand (via df.join)
//      // [X] Union All
//      // [X] Union Distinct (spark distinct vs distinct on rdd's with explicit orderability)
//      // [X] Optional match (via df.join with type left_outer)
//      // [X] UNWIND
//      // [X] Bounded var length (via UNION and single steps)
//      // [X] Unbounded var length (we think we can do it in a loop but it will be probably be really expensive)
//      // [-] Shortest path -- not easily on datasets/dataframes directly but possible via graphx
//
//      // [X] Aggregation (via rdd, possibly via spark if applicable given the available types)
//      // [X] CALL .. YIELD ... (via df.map + procedure registry)
//      // [X] Graph algorithms (map into procedures)
//
//      /* Updates
//
//        ... 2 3 4 2 2 ... => MERGE (a:A {id: id ...}
//
//        ... | MERGE 2
//        ... | MERGE 3
//        ... | MERGE 4
//        ... | MERGE 2
//        ... | MERGE 2
//
//        ... | CREATE-MERGE 2
//        ... | MATCH-MERGE 3
//        ... | CREATE-MERGE 4
//        ... | CREATE-MERGE 2
//        ... | CREATE-MERGE 2
//
//     */
//
//      // CypherFrames and expression evaluation (including null issues)
      case _ =>
        throw new UnsupportedOperationException("I don't want a NotImplemented warning")
    }
  }

  import org.opencypher.spark.impl.foo._

  override def cypherNew(ir: QueryRepresentation[Expr], params: Map[String, CypherValue]): CypherResultContainer = {

    implicit val pInner = planningContext
    implicit val runtimeContext = new StdRuntimeContext(session, params)

    val root = ir.root

    implicit val tokenDefs = root.tokens

    val first = root.solve

    val plan = first match {
      case MatchBlock(_, _, given, where, _) =>
        // plan given
        val plan = givenPlanner(given).asProduct
        // all variables are now projected to fields
        // and will be available to predicates
        val withFilters = wherePlanner(plan, where)

        withFilters
    }

    val finished = root.blocks.blocks.values.foldLeft(plan) {
      case (acc, next) => next match {
        case ProjectBlock(_, _, _, _, Yields(exprs), _) =>
          planProjections(acc, exprs)
        case ReturnBlock(_, BlockSignature(_, out), _) =>

          // all blocks planned, drop extra columns
          acc.selectFields(out.toSeq.map(f => Symbol(f.name.replaceAllLiterally(".", "_"))):_*)
        case _ => acc
      }
    }


    StdCypherResultContainer.fromProducts(finished)
  }

  private def planProjections(in: StdCypherFrame[Product], exprs: Set[Expr])(implicit tokens: TokenDefs) = {
    exprs.foldLeft(in) {
      case (acc, Property(Var(m), key)) =>
        acc.propertyValue(Symbol(m), Symbol(tokens.propertyKey(key).name))(prop(m, tokens.propertyKey(key)))
      case x => throw new UnsupportedOperationException(s"can not project $x")
    }
  }

  private def wherePlanner(in: StdCypherFrame[Product], where: Where[Expr])(implicit tokens: TokenDefs) = {
    val labels = where.predicates.collect {
      case HasLabel(Var(n), l) => n -> l
    }.toMap.groupBy(_._1).mapValues(_.values.toSet)

    val withLabelFilters = labels.foldLeft(in) {
      case (acc, (n, lbls)) =>
        FilterProduct.labelFilter(acc)(Symbol(n), lbls.map(tokens.label(_).name).toSeq)
    }

    val equalities = where.predicates.foldLeft(withLabelFilters) {
      case (acc, Equals(Property(Var(m), key), p: Param)) =>
        val withProp = acc.propertyValue(Symbol(m), Symbol(tokens.propertyKey(key).name))(prop(m, tokens.propertyKey(key)))
        FilterProduct.paramEqFilter(withProp)(withProp.projectedField.sym, p)
//        withProp
      case (acc, _: HasLabel) => acc
      case (acc, x) => throw new UnsupportedOperationException(s"Can't deal with $x")
    }

    equalities
//    withLabelFilters

  }

  private def givenPlanner(given: Given)(implicit tokens: TokenDefs) = {
    val nodeFrames: Set[ProjectFrame] = given.entities.collect { case x: AnyNode => x }.map(nodePlan)
    val rels = given.entities.collect { case x: AnyRelationship => x }.map(relPlan)

    val nodesToJoin = nodeFrames.map { frame =>
      frame.projectedField.sym -> frame.asRow
    }.toMap

    if (rels.size == 1) {
      val (s, (e, r), t) = rels.head
      val sFrame = nodesToJoin(s)
      val tFrame = nodesToJoin(t)

      r.join(sFrame).on(relStart(e))(s).join(tFrame).on(relEnd(e))(t)
    } else ???
  }

  private def nodePlan(e: AnyNode) =
    allNodes(e.entity).asProduct.nodeId(e.entity)(nodeId(e.entity))

  private def relPlan(e: AnyRelationship)(implicit tokens: TokenDefs) = {
    val plan = typeScan(e.entity)(e.typ.toIndexedSeq.map(ref => tokens.relType(ref).name))
      .asProduct.relationshipStartId(e.entity)(relStart(e)).relationshipEndId(e.entity)(relEnd(e))
    (nodeId(e.from), e -> plan.asRow, nodeId(e.to))
  }

  private def nodeId(n: Field): Symbol = Symbol(n.name + "_id")
  private def relId(r: AnyRelationship): Symbol = Symbol(r.entity.name + "_id")
  private def relStart(r: AnyRelationship): Symbol = Symbol(r.entity.name + "_start")
  private def relEnd(r: AnyRelationship): Symbol = Symbol(r.entity.name + "_end")
  private def prop(m: String, key: PropertyKeyDef) = Symbol(m + "_" + key.name)

}

object foo {
  import scala.languageFeature.implicitConversions._
  implicit def fieldToSym(f: Field): Symbol = Symbol(f.name)
}
