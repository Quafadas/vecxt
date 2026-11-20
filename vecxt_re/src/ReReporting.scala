package vecxt_re

import vecxt.all.*

/** Per-layer loss metrics over a simulation.
  *
  * Precondition for every method taking `years`: `years(i)` is the 1-based iteration of `cededToLayer(i)`, the two
  * arrays have the same length, `years` is sorted in non-decreasing order, and every value is in 1..numIterations.
  * Arrays that come from a `Scenario`, or a `Scenarr` with `isSorted = true`, already satisfy this.
  *
  * The precondition is NOT checked. These methods are on the reporting hot path and work in a single pass over arrays
  * with one entry per event, and checking would add a second full pass on every call. Unsorted `years` silently gives
  * wrong numbers, not an error: the group-sum based methods keep only the last run of each iteration (see
  * groupSums.scala), and `lossReport` drops every event after the first out-of-order one.
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
      *   Sorted array of 1-based iteration indices, one per element of `cededToLayer`. Not checked: see the object
      *   doc.
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
end ReReporting
