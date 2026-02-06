package vecxt_re

/** Hill estimator for Pareto tail index estimation.
  *
  * The Hill estimator is used to estimate the shape parameter (α) of a Pareto distribution from the upper tail of the
  * data. For a Pareto distribution with survival function S(x) = (x_min/x)^α, the tail index α determines how heavy the
  * tail is:
  *   - α < 2: Infinite variance
  *   - α < 1: Infinite mean
  *   - Larger α means lighter tails
  *
  * The estimator uses the k largest order statistics: α̂ = 1 / (1/k * Σᵢ₌₁ᵏ ln(X₍ₙ₋ᵢ₊₁₎) - ln(X₍ₙ₋ₖ₎))
  */
object HillEstimator:

  /** Computes the Hill estimator for the Pareto tail index using the k largest observations.
    *
    * @param data
    *   The data array (will be sorted internally)
    * @param k
    *   The number of upper order statistics to use (must be between 1 and n-1)
    * @return
    *   The estimated tail index α
    * @throws IllegalArgumentException
    *   if k is out of valid range or data is empty
    */
  def apply(data: Array[Double], k: Int): Double =
    require(data.length > 1, "Data must have at least 2 observations")
    require(k >= 1 && k < data.length, s"k must be between 1 and ${data.length - 1}, got $k")

    val sorted = data.clone()
    java.util.Arrays.sort(sorted)

    val n = sorted.length
    val threshold = sorted(n - k - 1) // X_(n-k)

    require(threshold > 0, "Threshold (k-th largest value) must be positive for Pareto estimation")

    var sumLogRatios = 0.0
    var i = 0
    while i < k do
      val xi = sorted(n - 1 - i) // X_(n-i+1) for i = 1..k
      sumLogRatios += math.log(xi) - math.log(threshold)
      i += 1
    end while

    k.toDouble / sumLogRatios
  end apply

  /** Result of a Hill plot computation containing k values and corresponding tail index estimates.
    *
    * @param kValues
    *   Array of k values used
    * @param estimates
    *   Corresponding tail index estimates α̂(k)
    */
  case class HillPlotResult(
      kValues: Array[Int],
      estimates: Array[Double]
  ):
    /** Find a stable region in the Hill plot by looking for low variance segments.
      *
      * @param windowSize
      *   Size of the sliding window for variance calculation
      * @param threshold
      *   Maximum coefficient of variation to consider "stable"
      * @return
      *   Optional tuple of (start k, end k, mean estimate) for the most stable region
      */
    def findStableRegion(
        windowSize: Int = 10,
        threshold: Double = 0.1
    ): Option[(bestStart: Int, bestEnd: Int, meanEstimate: Double)] =
      if kValues.length < windowSize then None
      else
        var bestVariance = Double.MaxValue
        var bestStart = 0
        var bestMean = 0.0

        var i = 0
        while i <= estimates.length - windowSize do
          var sum = 0.0
          var sumSq = 0.0
          var j = 0
          while j < windowSize do
            val v = estimates(i + j)
            sum += v
            sumSq += v * v
            j += 1
          end while
          val mean = sum / windowSize
          val variance = sumSq / windowSize - mean * mean
          val cv = if mean != 0 then math.sqrt(variance) / math.abs(mean) else Double.MaxValue

          if cv < bestVariance && cv < threshold then
            bestVariance = cv
            bestStart = i
            bestMean = mean
          end if
          i += 1
        end while

        if bestVariance < threshold then
          Some((bestStart = kValues(bestStart), bestEnd = kValues(bestStart + windowSize - 1), meanEstimate = bestMean))
        else None
        end if
      end if
    end findStableRegion
  end HillPlotResult

  /** Computes a Hill plot: tail index estimates for a range of k values.
    *
    * A Hill plot shows how the estimate varies with k. A good estimate should show a stable plateau region. Too small k
    * gives high variance; too large k includes non-tail observations.
    *
    * @param data
    *   The data array
    * @param kMin
    *   Minimum k value (default: 2)
    * @param kMax
    *   Maximum k value (default: n/2 or n-1, whichever is smaller)
    * @param step
    *   Step size for k values (default: 1)
    * @return
    *   HillPlotResult containing k values and corresponding estimates
    */
  def hillPlot(
      data: Array[Double],
      kMin: Int = 2,
      kMax: Int = -1,
      step: Int = 1
  ): HillPlotResult =
    require(data.length > 2, "Data must have at least 3 observations")

    val sorted = data.clone()
    java.util.Arrays.sort(sorted)

    val n = sorted.length
    val actualKMax = if kMax < 0 then math.min(n / 2, n - 1) else math.min(kMax, n - 1)
    val actualKMin = math.max(kMin, 1)

    require(actualKMin < actualKMax, s"kMin ($actualKMin) must be less than kMax ($actualKMax)")

    // Pre-compute log values for efficiency
    val logValues = new Array[Double](n)
    var i = 0
    while i < n do
      logValues(i) = math.log(sorted(i))
      i += 1
    end while

    // Calculate number of k values
    val numK = (actualKMax - actualKMin) / step + 1
    val kValues = new Array[Int](numK)
    val estimates = new Array[Double](numK)

    var idx = 0
    var k = actualKMin
    while k <= actualKMax do
      val threshold = sorted(n - k - 1)
      val logThreshold = logValues(n - k - 1)

      var sumLogRatios = 0.0
      var j = 0
      while j < k do
        sumLogRatios += logValues(n - 1 - j) - logThreshold
        j += 1
      end while

      kValues(idx) = k
      estimates(idx) = k.toDouble / sumLogRatios
      idx += 1
      k += step
    end while

    HillPlotResult(kValues, estimates)
  end hillPlot

end HillEstimator

object HillEstimatorExtensions:

  extension (vec: Array[Double])

    /** Computes the Hill estimator for the Pareto tail index.
      *
      * @param k
      *   The number of upper order statistics to use
      * @return
      *   The estimated tail index α
      */
    inline def hillEstimator(k: Int): Double = HillEstimator(vec, k)

    /** Computes a Hill plot for this data.
      *
      * @param kMin
      *   Minimum k value (default: 2)
      * @param kMax
      *   Maximum k value (default: n/2)
      * @param step
      *   Step size for k values (default: 1)
      * @return
      *   HillPlotResult with k values and estimates
      */
    inline def hillPlot(
        kMin: Int = 2,
        kMax: Int = -1,
        step: Int = 1
    ): HillEstimator.HillPlotResult =
      HillEstimator.hillPlot(vec, kMin, kMax, step)
  end extension

end HillEstimatorExtensions
