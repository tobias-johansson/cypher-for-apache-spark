package org.opencypher.okapi.types

import cats.kernel.Eq
import cats.{Id, InvariantMonoidal, Monoid}
import org.opencypher.okapi.types.Types._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

class UnionTypeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Discipline {

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

    (CTStruct("a" -> CTString, "b" -> CTInteger) | CTStruct("a" -> CTString, "b" -> CTInteger))
      .shouldEqual(CTStruct("a" -> CTString, "b" -> CTInteger))

    (CTStruct("a" -> CTString, "b" -> CTInteger) | CTStruct("a" -> CTString, "b" -> CTString))
      .shouldEqual(CTStruct("a" -> CTString, "b" -> CTUnion(CTInteger, CTString)))

    (CTStruct("a" -> CTString, "b" -> CTInteger) | CTStruct("a" -> CTString))
      .shouldEqual(CTStruct("a" -> CTString, "b" -> CTInteger.?))

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

    (CTStruct("a" -> CTString, "b" -> CTInteger) & CTStruct("a" -> CTString, "b" -> CTInteger))
      .shouldEqual(CTStruct("a" -> CTString, "b" -> CTInteger))

    (CTStruct("a" -> CTString) & CTStruct("b" -> CTString))
      .shouldEqual(CTVoid)

  }

  //
  //test("union laws") {
  //
  //  forAll(any, any) { (a, b) =>
  //    a | b shouldEqual b | a
  //  }
  //
  //  forAll(any) { a =>
  //    a | CTVoid shouldEqual a
  //  }
  //
  //  forAll(any, any) { (a, b) =>
  //    a | a shouldEqual a
  //  }
  //}
  //
  //test("intersection laws") {
  //
  //  forAll(any, any) { (a, b) =>
  //    a & b shouldEqual b & a
  //  }
  //
  //  forAll(any) { a =>
  //    a & CTVoid shouldEqual CTVoid
  //  }
  //
  //  forAll(any, any) { (a, b) =>
  //    a & a shouldEqual a
  //  }
  //}


}
