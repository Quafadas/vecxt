package vecxt_re

// TODO: RelVec(values: Array[Double], unit) for bulk ops, avoid Array[Rel]

/** Relative Units that have a unit attached.
  *
  * @param value
  * @param unit
  */
final case class Rel(relativeToPriceUnit: Double, unit: PriceUnit):
  def canonical: Double = unit.convert(relativeToPriceUnit, PriceUnit.One)
  def to(u: PriceUnit): Rel = Rel(unit.convert(relativeToPriceUnit, u), u)
  inline def value = canonical
  inline def in(u: PriceUnit) = to(u)

  def approxEq(y: Rel, tol: Double = 1e-12): Boolean =
    math.abs(canonical - y.canonical) <= tol
end Rel

extension (k: Double) def *[R <: Rel](r: R): R = r * k
end extension

object Rel:
  // Arithmetic preserves the static type of the left operand, so Price + Rel is a Price.
  // Rel is final, so every R <: Rel is a Rel (or an opaque alias of it) at runtime and the cast is safe.
  extension [R <: Rel](x: R)
    def +(y: Rel): R =
      Rel(x.relativeToPriceUnit + y.unit.convert(y.relativeToPriceUnit, x.unit), x.unit).asInstanceOf[R]
    def -(y: Rel): R =
      Rel(x.relativeToPriceUnit - y.unit.convert(y.relativeToPriceUnit, x.unit), x.unit).asInstanceOf[R]
    def unary_- : R = x.copy(relativeToPriceUnit = -x.relativeToPriceUnit).asInstanceOf[R]
    def *(k: Double): R = x.copy(relativeToPriceUnit = x.relativeToPriceUnit * k).asInstanceOf[R]
  end extension

  given Ordering[Rel] = Ordering.by[Rel, Double](_.canonical)
  lazy val one = Rel(1.0, PriceUnit.One)
end Rel

opaque type Spread <: Rel = Rel
object Spread:
  def apply(v: Double, u: PriceUnit): Spread = Rel(v, u)
  def from(r: Rel): Spread = r
  given CanEqual[Spread, Spread] = CanEqual.derived
  given Ordering[Spread] = summon[Ordering[Rel]]
end Spread

opaque type RiskFreeRate <: Rel = Rel
object RiskFreeRate:
  def apply(v: Double, u: PriceUnit): RiskFreeRate = Rel(v, u)
  def from(r: Rel): RiskFreeRate = r
  given CanEqual[RiskFreeRate, RiskFreeRate] = CanEqual.derived
  given Ordering[RiskFreeRate] = summon[Ordering[Rel]]
end RiskFreeRate

opaque type Price <: Rel = Rel
object Price:
  def apply(v: Double, u: PriceUnit): Price = Rel(v, u)
  def from(r: Rel): Price = r
  given CanEqual[Price, Price] = CanEqual.derived
  given Ordering[Price] = summon[Ordering[Rel]]
end Price
