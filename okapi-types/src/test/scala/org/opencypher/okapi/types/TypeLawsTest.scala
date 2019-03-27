package org.opencypher.okapi.types

import cats.Monoid
import cats.kernel.Eq
import org.opencypher.okapi.types.Types._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

class TypeLawsTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Discipline {


  def pickOne[T](gens: List[Gen[T]]): Gen[T] = for {
    i <- Gen.choose(0, gens.size - 1)
    t <- gens(i)
  } yield t

  val simples = List(
    Gen.const(CTVoid),
    Gen.const(CTNull),
    Gen.const(CTString),
    Gen.const(CTInteger),
    Gen.const(CTFloat)
  )

  val simple = pickOne(simples)

  val list = for {
    elem <- simple
  } yield CTList(elem)

  val field = for {
    name <- Gen.identifier
    elem <- simple
  } yield name -> elem

  val struct = for {
    fields <- Gen.mapOf(field)
  } yield CTStruct(fields)

  val all = List(
    simples,
    List(list, struct)
  ).flatten

  val any: Gen[CType] = pickOne(all)

  implicit val arbitraryCType: Arbitrary[CType] = Arbitrary(any)

  implicit val eq: Eq[CType] = Eq.fromUniversalEquals

  val unionMonoid: Monoid[CType] = new Monoid[CType] {
    def empty: CType = CTVoid
    def combine(x: CType, y: CType): CType = x | y
  }

  val intersectionMonoid: Monoid[CType] = new Monoid[CType] {
    def empty: CType = CTAny
    def combine(x: CType, y: CType): CType = x & y
  }


  checkAll("CType.union", cats.kernel.laws.discipline.MonoidTests[CType](unionMonoid).monoid)

  checkAll("CType.intersection", cats.kernel.laws.discipline.MonoidTests[CType](intersectionMonoid).monoid)


}
