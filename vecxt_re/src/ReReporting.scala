package vecxt_re

import vecxt.all.*

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
      *   Sorted array of 1-based iteration indices
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
