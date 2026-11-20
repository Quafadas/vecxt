package vecxt_re

import vecxt.all.*

/** Per-layer loss metrics over a simulation.
  *
  * Precondition for every method taking `years`: `years(i)` is the 1-based iteration of `cededToLayer(i)`, the two
  * arrays have the same length, `years` is sorted in non-decreasing order, and every value is in 1..numIterations.
  * Arrays that come from a `Scenario`, or a `Scenarr` with `isSorted = true`, already satisfy this.
  *
  * `lossReport` checks the precondition and throws IllegalArgumentException if it is broken. The check costs nothing
  * extra: its single pass only consumes `years(i)` when it equals the current iteration, so leftover entries at the end
  * mean `years` was unsorted or out of range.
  *
  * The group-sum based methods (`attachmentProbability`, `exhaustionProbability`, `std`) do NOT check it, because
  * checking would add a second full pass. Unsorted `years` silently gives wrong numbers there: they keep only the last
  * run of each iteration (see groupSums.scala).
  */
object ReReporting:
  extension (calcd: (layer: Layer, cededToLayer: Array[Double]))

    inline def attachmentProbability(numIterations: Int, years: Array[Int]): Double =
      (groupSum(years, calcd.cededToLayer, numIterations) > 0).trues / numIterations.toDouble

    inline def exhaustionProbability(numIterations: Int, years: Array[Int]): Double =
      val exhaust = calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) - 0.01
      (groupSum(years, calcd.cededToLayer, numIterations) > exhaust).trues / numIterations.toDouble
    end exhaustionProbability

    inline def expectedLoss(numIterations: Int): Double = calcd.cededToLayer.sumSIMD / numIterations

    inline def std(numIterations: Int, years: Array[Int]): Double =
      groupSum(years, calcd.cededToLayer, numIterations).stdDev

    /** Efficient single-pass loss report computation.
      *
      * This method computes all loss metrics (EL, std, attachment probability, exhaustion probability) in a single pass
      * through the data, using Welford's online algorithm for numerically stable variance computation.
      *
      * This is significantly more efficient than calling the individual metric methods separately, as it avoids
      * multiple iterations through the grouped sums.
      *
      * @param numIterations
      *   Number of simulation iterations
      * @param years
      *   Sorted array of 1-based iteration indices, one per element of `cededToLayer`. Checked: see the object doc.
      * @param limit
      *   Report denominator for normalizing EL and std
      * @return
      *   Named tuple with (name, limit, el, stdDev, attachProb, exhaustProb)
      */
    inline def lossReport(
        numIterations: Int,
        years: Array[Int],
        limit: ReportDenominator
    ): (name: String, limit: Double, el: Double, stdDev: Double, attachProb: Double, exhaustProb: Double) =
      PlatformReporting.lossReportFast(calcd, numIterations, years, limit)

  end extension

  /** Failure path for `PlatformReporting.lossReportFast`. Kept out of the inline method so the diagnostic code isn't
    * copied into every call site. Only runs once the precondition is already known to be broken.
    */
  private[vecxt_re] def yearsMismatch(years: Array[Int], numValues: Int, numIterations: Int): Nothing =
    if numValues != years.length then
      throw IllegalArgumentException(
        s"years and cededToLayer must have the same length: years=${years.length}, cededToLayer=$numValues"
      )
    end if
    var k = 0
    while k < years.length do
      val y = years(k)
      if y < 1 || y > numIterations then
        throw IllegalArgumentException(
          s"years must be 1-based iteration indices in 1..$numIterations, but years($k) = $y"
        )
      end if
      if k > 0 && y < years(k - 1) then
        throw IllegalArgumentException(
          s"years must be sorted in non-decreasing order, but years(${k - 1}) = ${years(k - 1)} > years($k) = $y. " +
            "Sort the scenario first (Scenarr.sorted), and use the iterations of the sorted scenario."
        )
      end if
      k += 1
    end while
    throw IllegalArgumentException("years violates the lossReport precondition")
  end yearsMismatch
end ReReporting
