package vecxt_re

/** Platform-specific reporting implementations for JS and Native.
  *
  * Uses a streaming single-pass algorithm to compute all loss metrics efficiently.
  */
object PlatformReporting:

  /** Computes loss report metrics in a single pass using Welford's online algorithm.
    *
    * Instead of calling groupSum multiple times (for attachment, exhaustion, std, and EL), this method iterates
    * through the grouped sums once, accumulating all intermediate results:
    *   - Sum for expected loss (EL)
    *   - Count of attached iterations (groupSum > 0)
    *   - Count of exhausted iterations (groupSum >= exhaust threshold)
    *   - Running mean and M2 for Welford's variance algorithm
    *
    * @param calcd
    *   Tuple of (layer, cededToLayer array)
    * @param numIterations
    *   Number of iterations/years
    * @param years
    *   Sorted array of 1-based iteration indices
    * @param limit
    *   Report denominator for normalizing results
    * @return
    *   Named tuple with all loss report metrics
    */
  inline def lossReportFast(
      calcd: (layer: Layer, cededToLayer: Array[Double]),
      numIterations: Int,
      years: Array[Int],
      limit: ReportDenominator
  ): (name: String, limit: Double, el: Double, stdDev: Double, attachProb: Double, exhaustProb: Double) =
    val reportLimit = limit.fromlayer(calcd.layer)
    val exhaust = calcd.layer.aggLimit.getOrElse(Double.PositiveInfinity) - 0.01
    val values = calcd.cededToLayer
    val l = years.length

    // Accumulators
    var totalSum = 0.0 // For expected loss
    var attachCount = 0 // Count of iterations with loss > 0
    var exhaustCount = 0 // Count of iterations at exhaustion
    // Welford's online algorithm accumulators
    var mean = 0.0
    var m2 = 0.0
    var n = 0 // Count for Welford (should equal numIterations at end)

    // Single pass through groups (similar to groupSum but computing all metrics)
    var i = 0
    var currentGroup = 1
    while currentGroup <= numIterations do
      var groupSum = 0.0

      // Sum all values in this group
      while i < l && years(i) == currentGroup do
        groupSum += values(i)
        i += 1
      end while

      // Update total sum for EL
      totalSum += groupSum

      // Update attachment count (any positive loss)
      if groupSum > 0 then attachCount += 1

      // Update exhaustion count
      if groupSum > exhaust then exhaustCount += 1

      // Welford's online algorithm for variance
      n += 1
      val delta = groupSum - mean
      mean += delta / n
      val delta2 = groupSum - mean
      m2 += delta * delta2

      currentGroup += 1
    end while

    // Compute final statistics
    val el = totalSum / numIterations
    val variance = if n > 0 then m2 / n else 0.0
    val stdDev = Math.sqrt(variance)
    val attachProb = attachCount.toDouble / numIterations
    val exhaustProb = exhaustCount.toDouble / numIterations

    (
      name = calcd.layer.layerName.getOrElse(s"Layer ${calcd.layer.layerId}"),
      limit = reportLimit,
      el = el / reportLimit,
      stdDev = stdDev / reportLimit,
      attachProb = attachProb,
      exhaustProb = exhaustProb
    )
  end lossReportFast
end PlatformReporting