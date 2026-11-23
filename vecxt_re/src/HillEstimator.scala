package vecxt_re

/** Which version of the Hill estimator to compute. All three are built from the k largest losses and the same
  * continuous-threshold curve; they differ in where the threshold sits and in the numerator.
  *
  * Write X₍₁₎ ≥ X₍₂₎ ≥ … for the losses, largest first, and S = Σᵢ₌₁ᵏ log(X₍ᵢ₎ / threshold).
  *
  *   - [[MaxLikelihood]]: threshold X₍ₖ₊₁₎, α̂ = k / S (Hill 1975; de Haan & Ferreira; Resnick). The maximum likelihood
  *     estimate given the threshold. For exact Pareto data S is Gamma(k, α), so E[α̂] = α · k / (k − 1).
  *   - [[Embrechts]]: threshold X₍ₖ₎, the k-th largest loss itself, α̂ = k / S (Embrechts, Klüppelberg & Mikosch,
  *     1997). The i = k term of S is zero, so S is Gamma(k − 1, α) and E[α̂] = α · k / (k − 2). Equal to (k / (k − 1))
  *     · the [[MaxLikelihood]] estimate at k − 1; always at least the [[MaxLikelihood]] estimate at the same k.
  *   - [[Unbiased]]: threshold X₍ₖ₊₁₎, α̂ = (k − 1) / S. Exactly unbiased for Pareto data, and minimum variance among
  *     unbiased estimators: the most conservative of the three.
  *
  * The conventions agree as k → ∞; they differ by 25% or more at k = 5. None corrects for losses that are not Pareto.
  */
enum HillConvention:
  case MaxLikelihood, Embrechts, Unbiased

  /** The smallest k the estimate is defined for. */
  def minK: Int = if this == MaxLikelihood then 1 else 2

  /** The default smallest k for a Hill plot: 2, or 3 for [[Embrechts]]. Embrechts at k = 2 is built from one log
    * spacing and has infinite mean for Pareto data; starting at 3 also gives every convention the same thresholds, from
    * the 3rd largest loss down.
    */
  def defaultKMin: Int = if this == Embrechts then 3 else 2

  /** The largest k the estimate is defined for, given n losses. */
  def maxK(n: Int): Int = if this == Embrechts then n else n - 1

  /** The number of losses at or above the threshold, minus k: 1 for the (k+1)-th largest, 0 for the k-th. */
  private[vecxt_re] def thresholdOffset: Int = if this == Embrechts then 0 else 1

  /** The numerator of the estimate over S. */
  private[vecxt_re] def numerator(k: Int): Double = if this == Unbiased then k - 1.0 else k.toDouble

  /** The number of non-zero log spacings in S, so that 2αS is χ² with twice this many degrees of freedom. */
  private[vecxt_re] def spacings(k: Int): Int = if this == Embrechts then k - 1 else k
end HillConvention

/** Hill estimator for Pareto tail index estimation.
  *
  * The Hill estimator is used to estimate the shape parameter (α) of a Pareto distribution from the upper tail of the
  * data. For a Pareto distribution with survival function S(x) = (x_min/x)^α, the tail index α determines how heavy the
  * tail is:
  *   - α < 2: Infinite variance
  *   - α < 1: Infinite mean
  *   - Larger α means lighter tails
  *
  * By default the estimator uses the k largest order statistics over the (k+1)-th: α̂ = k / Σᵢ₌₁ᵏ log(X₍ᵢ₎ / X₍ₖ₊₁₎).
  * See [[HillConvention]] for the alternatives.
  */
object HillEstimator:

  /** Computes the Hill estimator for the Pareto tail index using the k largest observations.
    *
    * @param data
    *   The data array (will be sorted internally)
    * @param k
    *   The number of upper order statistics to use, between `convention.minK` and `convention.maxK(n)`
    * @param convention
    *   Which estimator to compute, see [[HillConvention]]
    * @return
    *   The estimated tail index α
    * @throws IllegalArgumentException
    *   if k is out of valid range or data is empty
    */
  def apply(data: Array[Double], k: Int, convention: HillConvention = HillConvention.MaxLikelihood): Double =
    require(data.length > 1, "Data must have at least 2 observations")
    val (minK, maxK) = (convention.minK, convention.maxK(data.length))
    require(k >= minK && k <= maxK, s"k must be between $minK and $maxK for $convention, got $k")

    val sorted = data.clone()
    java.util.Arrays.sort(sorted)

    val n = sorted.length
    val threshold = sorted(n - k - convention.thresholdOffset)

    require(threshold > 0, "Threshold must be positive for Pareto estimation")

    var sumLogRatios = 0.0
    var i = 0
    while i < k do
      val xi = sorted(n - 1 - i) // the (i+1)-th largest
      sumLogRatios += math.log(xi) - math.log(threshold)
      i += 1
    end while

    convention.numerator(k) / sumLogRatios
  end apply

  /** Result of a Hill plot computation containing k values and corresponding tail index estimates.
    *
    * @param kValues
    *   Array of k values used
    * @param estimates
    *   Corresponding tail index estimates α̂(k)
    * @param thresholds
    *   The threshold behind each estimate: the (k+1)-th largest observation, or the k-th for
    *   [[HillConvention.Embrechts]]. Empty if not supplied.
    * @param nObs
    *   The number of observations n, so that k / n is the empirical exceedance probability of each threshold. 0 if not
    *   supplied.
    * @param convention
    *   Which estimator the estimates are, see [[HillConvention]]
    */
  case class HillPlotResult(
      kValues: Array[Int],
      estimates: Array[Double],
      thresholds: Array[Double] = Array.emptyDoubleArray,
      nObs: Int = 0,
      convention: HillConvention = HillConvention.MaxLikelihood
  ):
    /** The sum of log ratios S behind each estimate, recovered from the estimate. */
    private[vecxt_re] def sumLogRatios(i: Int): Double = convention.numerator(kValues(i)) / estimates(i)

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
    *   Minimum k value (default: `convention.defaultKMin`, 2, or 3 for [[HillConvention.Embrechts]]), raised to
    *   `convention.minK` if smaller
    * @param kMax
    *   Maximum k value (default: `convention.maxK(n)`, every loss: n − 1, or n for [[HillConvention.Embrechts]]),
    *   lowered to `convention.maxK(n)` if larger
    * @param step
    *   Step size for k values (default: 1)
    * @param convention
    *   Which estimator to compute, see [[HillConvention]]
    * @return
    *   HillPlotResult containing k values and corresponding estimates
    */
  def hillPlot(
      data: Array[Double],
      kMin: Int = -1,
      kMax: Int = -1,
      step: Int = 1,
      convention: HillConvention = HillConvention.MaxLikelihood
  ): HillPlotResult =
    require(data.length > 2, "Data must have at least 3 observations")

    val sorted = data.clone()
    java.util.Arrays.sort(sorted)

    val n = sorted.length
    val actualKMax = if kMax < 0 then convention.maxK(n) else math.min(kMax, convention.maxK(n))
    val actualKMin = if kMin < 0 then convention.defaultKMin else math.max(kMin, convention.minK)

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
    val thresholds = new Array[Double](numK)

    var idx = 0
    var k = actualKMin
    while k <= actualKMax do
      val t = n - k - convention.thresholdOffset
      val logThreshold = logValues(t)

      var sumLogRatios = 0.0
      var j = 0
      while j < k do
        sumLogRatios += logValues(n - 1 - j) - logThreshold
        j += 1
      end while

      kValues(idx) = k
      estimates(idx) = convention.numerator(k) / sumLogRatios
      thresholds(idx) = sorted(t)
      idx += 1
      k += step
    end while

    HillPlotResult(kValues, estimates, thresholds, n, convention)
  end hillPlot

end HillEstimator

object HillEstimatorExtensions:

  extension (vec: Array[Double])

    /** Computes the Hill estimator for the Pareto tail index.
      *
      * @param k
      *   The number of upper order statistics to use
      * @param convention
      *   Which estimator to compute, see [[HillConvention]]
      * @return
      *   The estimated tail index α
      */
    inline def hillEstimator(k: Int, convention: HillConvention = HillConvention.MaxLikelihood): Double =
      HillEstimator(vec, k, convention)

    /** Computes a Hill plot for this data.
      *
      * @param kMin
      *   Minimum k value (default: 2, or 3 for [[HillConvention.Embrechts]])
      * @param kMax
      *   Maximum k value (default: every loss, `convention.maxK(n)`)
      * @param step
      *   Step size for k values (default: 1)
      * @param convention
      *   Which estimator to compute, see [[HillConvention]]
      * @return
      *   HillPlotResult with k values and estimates
      */
    inline def hillPlot(
        kMin: Int = -1,
        kMax: Int = -1,
        step: Int = 1,
        convention: HillConvention = HillConvention.MaxLikelihood
    ): HillEstimator.HillPlotResult =
      HillEstimator.hillPlot(vec, kMin, kMax, step, convention)
  end extension

end HillEstimatorExtensions
