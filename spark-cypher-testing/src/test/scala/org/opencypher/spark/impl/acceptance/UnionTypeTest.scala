package org.opencypher.spark.impl.acceptance

import org.scalatest.{FunSuite, Matchers}

class UnionTypeTest extends FunSuite with Matchers {

  sealed trait CType {
    def |(that: CType): CType = union(this, that)
    def &(that: CType): CType = intersection(this, that)
    def ? : CType = union(this, CTNull)
  }

  case object CTVoid extends CType

  case object CTNull extends CType

  case object CTString extends CType

  case object CTInteger extends CType

  case object CTFloat extends CType

  case class CTList(element: CType) extends CType

  case class CTMap(fields: Map[String, CType]) extends CType {
    override def toString: String = productPrefix + "(" + fields.mkString(",") + ")"
  }

  object CTMap {
    def apply(fields: (String, CType)*): CTMap = CTMap(Map(fields: _*))
  }

  case class CTUnion(types: Set[CType]) extends CType
  object CTUnion {
    def apply(types: CType*): CTUnion = CTUnion(Set(types: _*))
  }

  def constituents(t: CType) = t match {
    case CTUnion(types) => types
    case st => Set(st)
  }

  def union(a: CType, b: CType): CType = (a, b) match {
    case (`a`, `a`) => a
    case (CTVoid, t) => t
    case (t, CTVoid) => t
    case (CTList(ea), CTList(eb)) =>
      CTList(union(ea, eb))
    case (CTMap(fa), CTMap(fb)) =>
      val fields = for {
        key <- fa.keySet ++ fb.keySet
        tpe = union(fa.getOrElse(key, CTNull), fb.getOrElse(key, CTNull))
      } yield key -> tpe
      CTMap(fields.toMap)
    case _ =>
      CTUnion(constituents(a) ++ constituents(b))
  }

  test("union") {

    (CTString | CTString)
      .shouldEqual(CTString)

    (CTString | CTInteger)
      .shouldEqual(CTUnion(CTString, CTInteger))

    (CTString | CTVoid)
      .shouldEqual(CTString)

    CTString.?
      .shouldEqual(CTUnion(CTString, CTNull))

    (CTString | CTInteger | CTFloat)
      .shouldEqual(CTUnion(CTString, CTInteger, CTFloat))

    (CTList(CTString) | CTList(CTString))
      .shouldEqual(CTList(CTString))

    (CTList(CTString) | CTList(CTInteger))
      .shouldEqual(CTList(CTString | CTInteger))

    (CTMap("a" -> CTString, "b" -> CTInteger) | CTMap("a" -> CTString, "b" -> CTInteger))
      .shouldEqual(CTMap("a" -> CTString, "b" -> CTInteger))

    (CTMap("a" -> CTString, "b" -> CTInteger) | CTMap("a" -> CTString, "b" -> CTString))
      .shouldEqual(CTMap("a" -> CTString, "b" -> CTUnion(CTInteger, CTString)))

    (CTMap("a" -> CTString, "b" -> CTInteger) | CTMap("a" -> CTString))
      .shouldEqual(CTMap("a" -> CTString, "b" -> CTInteger.?))

  }


  def intersection(a: CType, b: CType): CType = (a, b) match {
    case (`a`, `a`) => a
    case (CTList(ea), CTList(eb)) => CTList(intersection(ea, eb))
    case (CTMap(fa), CTMap(fb)) =>
      val fields = for {
        key <- fa.keySet ++ fb.keySet
        tpe = intersection(fa.getOrElse(key, CTVoid), fb.getOrElse(key, CTVoid))
        if tpe != CTVoid
      } yield key -> tpe
      CTMap(fields.toMap)
    case _ =>
      val types = constituents(a) intersect constituents(b)
      types.size match {
        case 0 => CTVoid
        case 1 => types.head
        case _ => CTUnion(types)
      }
  }

  test("intersection") {


    (CTString & CTString)
      .shouldEqual(CTString)

    (CTString & CTInteger)
      .shouldEqual(CTVoid)

    (CTString & CTVoid)
      .shouldEqual(CTVoid)

    (CTString.? & CTString.?)
      .shouldEqual(CTString.?)

    (CTString.? & CTString)
      .shouldEqual(CTString)

    (CTString.? & CTNull)
      .shouldEqual(CTNull)

    ((CTString | CTInteger | CTFloat) & (CTString | CTInteger))
      .shouldEqual(CTString | CTInteger)

    (CTList(CTString) & CTList(CTString))
      .shouldEqual(CTList(CTString))

    (CTList(CTString) & CTList(CTInteger))
      .shouldEqual(CTList(CTVoid))

    (CTMap("a" -> CTString, "b" -> CTInteger) & CTMap("a" -> CTString, "b" -> CTInteger))
      .shouldEqual(CTMap("a" -> CTString, "b" -> CTInteger))

    (CTMap("a" -> CTString) & CTMap("b" -> CTString))
      .shouldEqual(CTVoid)

    //(CTMap("a" -> CTString, "b" -> CTInteger) & CTMap("a" -> CTString, "b" -> CTString))
    //  .shouldEqual(CTMap("a" -> CTString))
    //
    //(CTMap("a" -> CTString, "b" -> CTInteger) | CTMap("a" -> CTString))
    //  .shouldEqual(CTMap("a" -> CTString, "b" -> CTInteger.?))



  }

}
