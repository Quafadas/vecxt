package vecxt_re

import scala.annotation.targetName

/** A price level relative to par, stored as a canonical Double where `1.0` is par.
  *
  * A Price is a point on a scale rather than an amount, so it is deliberately not a [[Rel]]: the difference of two
  * prices is a Rel, a Rel can be added to or subtracted from a Price, and two prices cannot be added together.
  *
  * As with Rel, [[PriceUnit]] only affects construction (`Price(102, PriceUnit.Pts)`) and display
  * (`p.in(PriceUnit.Pts)`).
  */
opaque type Price = Double

object Price:
  /** e.g. `Price(102, PriceUnit.Pts)` is 1.02 */
  def apply(v: Double, u: PriceUnit): Price = u.convert(v, PriceUnit.One)

  val par: Price = 1.0

  extension (x: Price)
    /** The canonical value, where `1.0` is par. */
    inline def canonical: Double = x

    /** The price expressed in unit `u`, e.g. `Price(1.02, PriceUnit.One).in(PriceUnit.Pts) == 102.0` */
    def in(u: PriceUnit): Double = PriceUnit.One.convert(x, u)

    def approxEq(y: Price, tol: Double = 1e-12): Boolean = math.abs(x - y) <= tol

    def +(r: Rel): Price = x + r.canonical
    def -(r: Rel): Price = x - r.canonical

    /** The change in price from `y` to `x`. */
    @targetName("minusPrice")
    def -(y: Price): Rel = Rel.fromCanonical(x - y)

    /** The ratio of two prices, e.g. a gross return. */
    def /(y: Price): Double = x / y
  end extension

  // Elementwise array versions, delegating to vecxt's Array[Double] operations. An Array[Price] is a double[] at
  // runtime, so `canonical` is a cast, not a copy. vecxt is called by its qualified name: importing its operators here
  // would clash with the ones defined above.
  extension (xs: Array[Price])
    @targetName("canonicalArr")
    def canonical: Array[Double] = xs

    @targetName("plusArr")
    def +(ys: Array[? <: Rel]): Array[Price] = vecxt.all.+(xs)(ys.canonical)

    @targetName("minusArr")
    def -(ys: Array[? <: Rel]): Array[Price] = vecxt.all.-(xs)(ys.canonical)

    @targetName("minusPriceArr")
    def -(ys: Array[Price]): Array[Rel] = Rel.fromCanonical[Rel](vecxt.all.-(xs)(ys))

    @targetName("divPriceArr")
    def /(ys: Array[Price]): Array[Double] = vecxt.all./(xs)(ys)
  end extension

  given CanEqual[Price, Price] = CanEqual.derived
  given Ordering[Price] = Ordering.Double.TotalOrdering
end Price
