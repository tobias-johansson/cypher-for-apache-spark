package org.opencypher.okapi.types

object Types {

  sealed trait CType {
    def |(that: CType): CType = union(this, that)
    def &(that: CType): CType = intersection(this, that)
    def ? : CType = union(this, CTNull)
    def constituents: Set[CType] = Set(this)
  }

  case object CTVoid extends CType

  case object CTAny extends CType

  case object CTNull extends CType

  case object CTString extends CType

  case object CTInteger extends CType

  case object CTFloat extends CType

  case class CTList(
    element: CType
  ) extends CType

  case class CTStruct(
    fields: Map[String, CType]
  ) extends CType {
    override def toString: String = productPrefix + "(" + fields.mkString(",") + ")"
  }

  object CTStruct {
    def apply(fields: (String, CType)*): CTStruct = CTStruct(Map(fields: _*))
  }

  case class CTUnion(types: Set[CType]) extends CType {
    override def constituents: Set[CType] = types
  }

  object CTUnion {
    def apply(types: CType*): CTUnion = CTUnion(Set(types: _*))
  }

  private def union(a: CType, b: CType): CType = (a, b) match {
    case (`a`, `a`) => a
    case (CTVoid, t) => t
    case (t, CTVoid) => t
    case (CTAny, _) => CTAny
    case (_, CTAny) => CTAny
    case (CTList(ea), CTList(eb)) =>
      CTList(ea | eb)
    case (CTStruct(fa), CTStruct(fb)) =>
      val fields = for {
        key <- fa.keySet union fb.keySet
        tpe = fa.getOrElse(key, CTNull) | fb.getOrElse(key, CTNull)
        if tpe != CTVoid
      } yield key -> tpe
      CTStruct(fields.toMap)
    case _ =>
      CTUnion(a.constituents union b.constituents)
  }

  private def intersection(a: CType, b: CType): CType = (a, b) match {
    case (`a`, `a`) => a
    case (CTAny, t) => t
    case (t, CTAny) => t
    case (CTList(ea), CTList(eb)) => CTList(ea & eb)
    case (CTStruct(fa), CTStruct(fb)) =>
      val fields = for {
        key <- fa.keySet intersect fb.keySet
        tpe = fa.getOrElse(key, CTVoid) & fb.getOrElse(key, CTVoid)
        if tpe != CTVoid
      } yield key -> tpe
      CTStruct(fields.toMap)
    case _ =>
      val types = a.constituents intersect b.constituents
      types.size match {
        case 0 => CTVoid
        case 1 => types.head
        case _ => CTUnion(types)
      }
  }

}
