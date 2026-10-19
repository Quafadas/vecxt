package vecxt_re

import scala.annotation.targetName

/** A relative quantity (a price relative to par, a rate, a spread, a ratio), stored as a canonical Double where `1.0`
  * is par / 100%.
  *
  * [[PriceUnit]] is a boundary concern only: it is used to construct a Rel from a quote (`Rel(102, PriceUnit.Pts)`) and
  * to read one back out (`r.in(PriceUnit.Bps)`). It is not stored, so equality, ordering and arithmetic are all on the
  * canonical value, and an `Array[Rel]` is an `Array[Double]` at runtime.
  */
opaque type Rel = Double

extension (k: Double) def *[R <: Rel](r: R): R = Rel.scale(r, k)
end extension

object Rel:
  /** e.g. `Rel(102, PriceUnit.Pts)` is 1.02 and `Rel(50, PriceUnit.Bps)` is 0.005 */
  def apply(v: Double, u: PriceUnit): Rel = u.convert(v, PriceUnit.One)

  val one: Rel = 1.0

  inline private[vecxt_re] def scale[R <: Rel](r: R, k: Double): R = (r * k).asInstanceOf[R]

  extension (x: Rel)
    /** The canonical value, where `1.0` is par / 100%. */
    inline def canonical: Double = x

    /** The value expressed in unit `u`, e.g. `Rel(102, PriceUnit.Pts).in(PriceUnit.Bps) == 10200.0` */
    def in(u: PriceUnit): Double = PriceUnit.One.convert(x, u)

    def approxEq(y: Rel, tol: Double = 1e-12): Boolean = math.abs(x - y) <= tol
  end extension

  // Arithmetic preserves the static type of the left operand, so Price + Rel is a Price.
  // Every R <: Rel is a Double at runtime, so the casts are free.
  extension [R <: Rel](x: R)
    def +(y: Rel): R = (x + y).asInstanceOf[R]
    def -(y: Rel): R = (x - y).asInstanceOf[R]
    def unary_- : R = (-x).asInstanceOf[R]
    def *(k: Double): R = (x * k).asInstanceOf[R]
    def /(k: Double): R = (x / k).asInstanceOf[R]
    // The ratio of two Rels is dimensionless, so a plain Double is returned.
    @targetName("divRel")
    def /(y: Rel): Double = x / y
  end extension

  given CanEqual[Rel, Rel] = CanEqual.derived
  // One generic instance for Rel and all its subtypes; per-subtype instances are ambiguous for e.g. `sorted[B >: Price]`.
  given [R <: Rel]: Ordering[R] = Ordering.Double.TotalOrdering.asInstanceOf[Ordering[R]]
end Rel

opaque type Spread <: Rel = Rel
object Spread:
  def apply(v: Double, u: PriceUnit): Spread = Rel(v, u)
  def from(r: Rel): Spread = r
  given CanEqual[Spread, Spread] = CanEqual.derived
end Spread

opaque type RiskFreeRate <: Rel = Rel
object RiskFreeRate:
  def apply(v: Double, u: PriceUnit): RiskFreeRate = Rel(v, u)
  def from(r: Rel): RiskFreeRate = r
  given CanEqual[RiskFreeRate, RiskFreeRate] = CanEqual.derived
end RiskFreeRate

opaque type Price <: Rel = Rel
object Price:
  def apply(v: Double, u: PriceUnit): Price = Rel(v, u)
  def from(r: Rel): Price = r
  given CanEqual[Price, Price] = CanEqual.derived
end Price
