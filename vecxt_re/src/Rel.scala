package vecxt_re

import scala.annotation.targetName

/** A relative quantity: a change in price, a return, a ratio, or a one year rate (see [[Spread]], [[RiskFreeRate]]).
  * Stored as a canonical Double where `1.0` is 100%.
  *
  * [[PriceUnit]] is a boundary concern only: it is used to construct a Rel from a quote (`Rel(50, PriceUnit.Bps)`) and
  * to read one back out (`r.in(PriceUnit.Bps)`). It is not stored, so equality, ordering and arithmetic are all on the
  * canonical value, and an `Array[Rel]` is an `Array[Double]` at runtime.
  *
  * A price level is not a Rel; see [[Price]]. `Price - Price` is a Rel and `Price + Rel` is a Price.
  */
opaque type Rel = Double

extension (k: Double) def *[R <: Rel](r: R): R = Rel.scale(r, k)
end extension

object Rel:
  /** e.g. `Rel(102, PriceUnit.Pts)` is 1.02 and `Rel(50, PriceUnit.Bps)` is 0.005 */
  def apply(v: Double, u: PriceUnit): Rel = u.convert(v, PriceUnit.One)

  val one: Rel = 1.0

  inline private[vecxt_re] def fromCanonical(d: Double): Rel = d
  inline private[vecxt_re] def fromCanonical[R <: Rel](ds: Array[Double]): Array[R] = ds.asInstanceOf[Array[R]]
  inline private[vecxt_re] def scale[R <: Rel](r: R, k: Double): R = (r * k).asInstanceOf[R]

  extension (x: Rel)
    /** The canonical value, where `1.0` is 100%. */
    inline def canonical: Double = x

    /** The value expressed in unit `u`, e.g. `Rel(0.005, PriceUnit.One).in(PriceUnit.Bps) == 50.0` */
    def in(u: PriceUnit): Double = PriceUnit.One.convert(x, u)

    def approxEq(y: Rel, tol: Double = 1e-12): Boolean = math.abs(x - y) <= tol
  end extension

  // Combining two values of the same type keeps that type (Spread + Spread is a Spread); mixing types widens to Rel
  // (Spread + RiskFreeRate is a Rel). Every R <: Rel is a Double at runtime, so the casts are free.
  extension [R <: Rel](x: R)
    def +(y: R): R = (x + y).asInstanceOf[R]
    def -(y: R): R = (x - y).asInstanceOf[R]
    def unary_- : R = (-x).asInstanceOf[R]
    def *(k: Double): R = (x * k).asInstanceOf[R]
    def /(k: Double): R = (x / k).asInstanceOf[R]
    // The ratio of two Rels is dimensionless, so a plain Double is returned.
    @targetName("divRel")
    def /(y: Rel): Double = x / y
  end extension

  // Elementwise array versions of the above, delegating to vecxt's Array[Double] operations. An Array[R] is a double[] at
  // runtime, so `canonical` is a cast, not a copy, and the same-type / mixed-type rules match the scalar operations.
  // vecxt is called by its qualified name: importing its operators here would clash with the ones defined above.
  extension (xs: Array[? <: Rel])
    @targetName("canonicalArr")
    inline def canonical: Array[Double] = xs.asInstanceOf[Array[Double]]
  end extension

  extension [R <: Rel](xs: Array[? <: R])
    @targetName("plusArr")
    def +(ys: Array[? <: R]): Array[R] = fromCanonical[R](vecxt.all.+(xs.canonical)(ys.canonical))

    @targetName("minusArr")
    def -(ys: Array[? <: R]): Array[R] = fromCanonical[R](vecxt.all.-(xs.canonical)(ys.canonical))

    @targetName("timesArr")
    def *(k: Double): Array[R] = fromCanonical[R](vecxt.all.*(xs.canonical)(k))
  end extension

  given CanEqual[Rel, Rel] = CanEqual.derived
  // One generic instance for Rel and all its subtypes; per-subtype instances are ambiguous for e.g. `sorted[B >: Spread]`.
  given [R <: Rel]: Ordering[R] = Ordering.Double.TotalOrdering.asInstanceOf[Ordering[R]]
end Rel

/** An annual credit spread. Over the one year horizon it is the return earned for bearing credit risk. */
opaque type Spread <: Rel = Rel
object Spread:
  def apply(v: Double, u: PriceUnit): Spread = Rel(v, u)
  def from(r: Rel): Spread = r
  given CanEqual[Spread, Spread] = CanEqual.derived
end Spread

/** An annual risk free rate. Over the one year horizon it is the risk free return. */
opaque type RiskFreeRate <: Rel = Rel
object RiskFreeRate:
  def apply(v: Double, u: PriceUnit): RiskFreeRate = Rel(v, u)
  def from(r: Rel): RiskFreeRate = r
  given CanEqual[RiskFreeRate, RiskFreeRate] = CanEqual.derived
end RiskFreeRate
