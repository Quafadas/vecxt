package vecxt_re

/** Which exceedance curve of a [[Scenarr]] to use. */
enum ExceedanceType(val title: String):
  /** Occurrence exceedance: the largest single event in each iteration. */
  case OEP extends ExceedanceType("OEP")

  /** Aggregate exceedance: the sum of all events in each iteration. */
  case AEP extends ExceedanceType("AEP")

  def of(scenario: Scenarr): ExceedanceCurve = this match
    case OEP => scenario.oep
    case AEP => scenario.aep
end ExceedanceType

/** An empirical exceedance curve: one value per iteration, ranked largest first.
  *
  * The `k`-th largest value (1-based rank `k`) sits at return period `numberIterations / k`: it is equalled or exceeded
  * in `k` of the `numberIterations` iterations. The curve is a step function between these points; nothing is
  * interpolated.
  *
  * @param amounts
  *   one value per iteration, sorted descending. Not copied and not checked: build one through `Scenarr.oep` /
  *   `Scenarr.aep`, or [[ExceedanceCurve.fromIterationValues]], unless you know the array is already ranked.
  */
final class ExceedanceCurve(val amounts: Array[Double]):

  /** The number of iterations, which is also the largest return period on the curve. */
  inline def numberIterations: Int = amounts.length

  /** Return period of the value at 0-based index `i`. */
  inline def returnPeriod(i: Int): Double = amounts.length.toDouble / (i + 1).toDouble

  lazy val returnPeriods: Array[Double] = Array.tabulate(amounts.length)(returnPeriod)

  /** The curve as `(returnPeriod, amount)` pairs, largest amount first. */
  lazy val points: Array[(returnPeriod: Double, amount: Double)] =
    Array.tabulate(amounts.length)(i => (returnPeriod = returnPeriod(i), amount = amounts(i)))

  /** The loss at return period `rp`: the `k`-th largest value with `k = floor(numberIterations / rp)`, i.e. the value
    * at the first curve point whose return period is at least `rp`. Off the grid of return periods this is the next
    * point up the curve, so it never understates the loss.
    *
    * Return periods below 1 give the smallest value.
    *
    * @throws IllegalArgumentException
    *   if `rp` is not positive, or exceeds `numberIterations`: the simulation says nothing about losses that rare.
    */
  def lossAt(rp: Double): Double =
    require(rp > 0, s"Return period must be positive, got $rp")
    require(
      rp <= numberIterations,
      s"Return period $rp is beyond the curve, which has $numberIterations iterations"
    )
    // k largest values have return period >= rp. Guard against floor(n / (n / k)) landing on k - 1 in floating point.
    var k = math.min(math.floor(numberIterations / rp).toInt, numberIterations)
    if k < numberIterations && returnPeriod(k) >= rp then k += 1
    end if
    amounts(k - 1)
  end lossAt

  /** The return period at which `loss` is reached or equalled: `numberIterations / (number of iterations >= loss)`.
    * `None` when no iteration reaches it.
    */
  def returnPeriodAt(loss: Double): Option[Double] =
    // amounts is descending: binary search for the number of values >= loss
    var lo = 0
    var hi = amounts.length
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if amounts(mid) >= loss then lo = mid + 1 else hi = mid
      end if
    end while
    Option.when(lo > 0)(amounts.length.toDouble / lo)
  end returnPeriodAt

  override def toString: String =
    val head = points.take(5).map(p => f"${p.returnPeriod}%.1f -> ${p.amount}%.4g").mkString(", ")
    s"ExceedanceCurve($numberIterations iterations: $head${if numberIterations > 5 then ", ..." else ""})"
  end toString
end ExceedanceCurve

object ExceedanceCurve:
  /** Rank one value per iteration (e.g. annual maxima or annual totals) into a curve. `values` is not modified. */
  def fromIterationValues(values: Array[Double]): ExceedanceCurve =
    val ranked = java.util.Arrays.copyOf(values, values.length)
    java.util.Arrays.sort(ranked)
    var i = 0
    var j = ranked.length - 1
    while i < j do
      val t = ranked(i)
      ranked(i) = ranked(j)
      ranked(j) = t
      i += 1
      j -= 1
    end while
    ExceedanceCurve(ranked)
  end fromIterationValues
end ExceedanceCurve
