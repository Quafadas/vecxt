package vecxt_re

import vecxt.all.*

/** Per-period rate changes, for on-leveling period-based models.
  *
  * Each entry is the _change factor_ for a period (e.g. 1.05 for +5%), counted from period 0 (current) backwards. The
  * cumulative factor which on-levels a value from a historical period to the current period is the product of the
  * factors in between.
  *
  * @param factors
  *   Array of change factors where factors(0) is the current period (period 0) and factors(n) is n periods back.
  */
case class RateChangesPerPeriod(factors: Array[Double]):

  /** Precomputed cumulative factors: cumulativeFactorsAll(i) = product of factors(0) to factors(i-1) */
  private lazy val cumulativeFactorsAll: Array[Double] =
    // cumulative product via exp(cumsum(log(x)))
    // Prepend 1.0 to get array where result(0) = 1.0, result(1) = factors(0), result(2) = factors(0)*factors(1), etc.
    val cumProd = factors.log
    cumProd.`cumsum!`
    cumProd.`exp!`
    // Prepend 1.0 for period 0 (current period needs no adjustment)
    Array.tabulate(factors.length + 1)(i => if i == 0 then 1.0 else cumProd(i - 1))
  end cumulativeFactorsAll

  /** Number of periods available */
  inline def numPeriods: Int = factors.length

  /** Get the change factor for a specific period.
    *
    * @param period
    *   The period number (0 = current, 1 = one period back, etc.)
    * @return
    *   The change factor for that period
    * @throws IndexOutOfBoundsException
    *   if period is outside the available range
    */
  inline def factorAt(period: Int): Double = factors(period)

  /** Calculate the cumulative on-leveling factor from a historical period to the current period.
    *
    * This multiplies all factors from period 0 up to (but not including) the specified historical period. The result is
    * the factor needed to bring a value from the historical period to current levels.
    *
    * For example, if you have rate changes of 5% (1.05) each year for 3 years:
    *   - factors = Array(1.05, 1.05, 1.05)
    *   - cumulativeToCurrentFrom(0) = 1.0 (already current)
    *   - cumulativeToCurrentFrom(1) = 1.05 (one period back, need to apply current period's change)
    *   - cumulativeToCurrentFrom(2) = 1.05 * 1.05 = 1.1025
    *   - cumulativeToCurrentFrom(3) = 1.05 * 1.05 * 1.05 = 1.157625
    *
    * @param fromPeriod
    *   The historical period number (0 = current, positive = periods back)
    * @return
    *   The cumulative factor to on-level from that period to current
    */
  inline def cumulativeToCurrentFrom(fromPeriod: Int): Double =
    if fromPeriod <= 0 then 1.0
    else if fromPeriod >= cumulativeFactorsAll.length then cumulativeFactorsAll.last
    else cumulativeFactorsAll(fromPeriod)
  end cumulativeToCurrentFrom

  /** Calculate cumulative on-leveling factors for all periods up to a given period.
    *
    * @param upToPeriod
    *   The maximum period to calculate (exclusive)
    * @return
    *   Array where result(i) is the cumulative factor from period i to current
    */
  inline def cumulativeFactors(upToPeriod: Int): Array[Double] =
    val n = math.min(upToPeriod, cumulativeFactorsAll.length)
    if n == cumulativeFactorsAll.length then cumulativeFactorsAll.clone()
    else Array.tabulate(n)(i => cumulativeFactorsAll(i))
    end if
  end cumulativeFactors

  /** Apply on-leveling to an array of values, given their corresponding period labels.
    *
    * @param values
    *   The historical values to on-level
    * @param periods
    *   The period label for each value (same length as values)
    * @return
    *   Array of on-leveled values
    */
  inline def onLevel(values: Array[Double], periods: Array[Int]): Array[Double] =
    require(values.length == periods.length, "values and periods must have the same length")
    // Map periods to cumulative factors, clamping to valid range
    val factors: Array[Double] = Array.tabulate(periods.length) { i =>
      val p = periods(i)
      if p <= 0 then 1.0
      else if p >= cumulativeFactorsAll.length then cumulativeFactorsAll.last
      else cumulativeFactorsAll(p)
      end if
    }
    values * (factors: Array[Double])
  end onLevel

end RateChangesPerPeriod

object RateChangesPerPeriod:

  /** Create an RateChangesPerPeriod from an array of rate changes (as percentages).
    *
    * @param rateChanges
    *   Array of rate changes where each value is the percentage change. e.g., 5.0 means a 5% increase, -3.0 means a 3%
    *   decrease.
    * @return
    *   RateChangesPerPeriod with the rate changes converted to factors
    */
  inline def fromRateChanges(rateChanges: Array[Double]): RateChangesPerPeriod =
    RateChangesPerPeriod((rateChanges / 100.0) + 1.0)
  end fromRateChanges

  /** Create an RateChangesPerPeriod with a constant rate change for all periods.
    *
    * @param numPeriods
    *   Number of historical periods
    * @param factor
    *   The constant factor for each period (e.g., 1.05 for 5% per period)
    * @return
    *   RateChangesPerPeriod with constant factors
    */
  inline def constant(numPeriods: Int, factor: Double): RateChangesPerPeriod =
    RateChangesPerPeriod(Array.fill(numPeriods)(factor))
  end constant

end RateChangesPerPeriod
