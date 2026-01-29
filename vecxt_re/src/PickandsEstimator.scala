package vecxt_re

/** Pickands estimator for extreme value index (tail index) estimation.
  *
  * It is unlikely to be useful, given the amount of data typically available in reinsurance.
  *
  * The Pickands estimator is a robust, non-parametric estimator for the extreme value index (EVI) of a distribution.
  * Unlike the Hill estimator which assumes a Pareto-type tail, the Pickands estimator works for all three domains of
  * attraction (Fréchet, Gumbel, Weibull).
  *
  * For heavy-tailed distributions (Fréchet domain), the EVI γ > 0 corresponds to a Pareto tail index α = 1/γ.
  *
  * The estimator uses order statistics at positions n-k, n-2k, n-4k: γ̂ = (1/ln2) * ln((X₍ₙ₋ₖ₎ - X₍ₙ₋₂ₖ₎) / (X₍ₙ₋₂ₖ₎ -
  * X₍ₙ₋₄ₖ₎))
  *
  * Properties:
  *   - More robust to model misspecification than Hill
  *   - Higher variance than Hill for pure Pareto data
  *   - Works for all extreme value distributions, not just Pareto
  *   - Consistent and asymptotically normal
  */
object PickandsEstimator:

  private val ln2 = math.log(2.0)

  /** Computes the Pickands estimator for the extreme value index.
    *
    * @param data
    *   The data array (will be sorted internally)
    * @param k
    *   The tuning parameter (must satisfy 4k < n)
    * @return
    *   The estimated extreme value index γ (for Pareto, α = 1/γ)
    * @throws IllegalArgumentException
    *   if k is out of valid range or data is too small
    */
  def apply(data: Array[Double], k: Int): Double =
    require(data.length >= 5, "Data must have at least 5 observations")
    require(k >= 1, s"k must be at least 1, got $k")
    require(4 * k < data.length, s"4*k must be less than n=${data.length}, got 4*$k=${4 * k}")

    val sorted = data.clone()
    java.util.Arrays.sort(sorted)

    val n = sorted.length

    // Order statistics (using 1-based indexing convention, converted to 0-based)
    // X_(n-k+1), X_(n-2k+1), X_(n-4k+1) in 1-based
    // = sorted(n-k), sorted(n-2k), sorted(n-4k) in 0-based
    val x_nk = sorted(n - k)
    val x_n2k = sorted(n - 2 * k)
    val x_n4k = sorted(n - 4 * k)

    val numerator = x_nk - x_n2k
    val denominator = x_n2k - x_n4k

    require(denominator > 0, "Denominator (X_(n-2k) - X_(n-4k)) must be positive")
    require(numerator > 0, "Numerator (X_(n-k) - X_(n-2k)) must be positive")

    val ratio = numerator / denominator
    math.log(ratio) / ln2
  end apply

  /** Computes the Pareto tail index α from the Pickands estimate.
    *
    * For heavy-tailed distributions in the Fréchet domain, γ > 0 and α = 1/γ.
    *
    * @param data
    *   The data array
    * @param k
    *   The tuning parameter
    * @return
    *   The estimated Pareto tail index α = 1/γ
    */
  def tailIndex(data: Array[Double], k: Int): Double =
    val gamma = apply(data, k)
    require(gamma > 0, s"Pickands estimate γ=$gamma is not positive; data may not be heavy-tailed")
    1.0 / gamma
  end tailIndex

  /** Result of a Pickands plot computation.
    *
    * @param kValues
    *   Array of k values used
    * @param gammaEstimates
    *   Corresponding EVI estimates γ̂(k)
    * @param alphaEstimates
    *   Corresponding tail index estimates α̂(k) = 1/γ̂(k) (NaN if γ ≤ 0)
    */
  case class PickandsPlotResult(
      kValues: Array[Int],
      gammaEstimates: Array[Double],
      alphaEstimates: Array[Double]
  ):
    /** Find a stable region by looking for low variance segments in gamma estimates.
      *
      * @param windowSize
      *   Size of the sliding window
      * @param threshold
      *   Maximum coefficient of variation to consider "stable"
      * @return
      *   Optional tuple of (start k, end k, mean gamma, mean alpha)
      */
    def findStableRegion(windowSize: Int = 5, threshold: Double = 0.2): Option[(Int, Int, Double, Double)] =
      if kValues.length < windowSize then None
      else
        var bestVariance = Double.MaxValue
        var bestStart = 0
        var bestMeanGamma = 0.0

        var i = 0
        while i <= gammaEstimates.length - windowSize do
          var sum = 0.0
          var sumSq = 0.0
          var validCount = 0
          var j = 0
          while j < windowSize do
            val v = gammaEstimates(i + j)
            if !v.isNaN && v.isFinite then
              sum += v
              sumSq += v * v
              validCount += 1
            end if
            j += 1
          end while

          if validCount == windowSize then
            val mean = sum / windowSize
            val variance = sumSq / windowSize - mean * mean
            val cv = if mean != 0 then math.sqrt(math.abs(variance)) / math.abs(mean) else Double.MaxValue

            if cv < bestVariance && cv < threshold then
              bestVariance = cv
              bestStart = i
              bestMeanGamma = mean
            end if
          end if
          i += 1
        end while

        if bestVariance < threshold && bestMeanGamma > 0 then
          Some((kValues(bestStart), kValues(bestStart + windowSize - 1), bestMeanGamma, 1.0 / bestMeanGamma))
        else None
        end if
      end if
    end findStableRegion
  end PickandsPlotResult

  /** Computes a Pickands plot: EVI estimates for a range of k values.
    *
    * @param data
    *   The data array
    * @param kMin
    *   Minimum k value (default: 1)
    * @param kMax
    *   Maximum k value (default: (n-1)/4)
    * @param step
    *   Step size for k values (default: 1)
    * @return
    *   PickandsPlotResult containing k values and estimates
    */
  def pickandsPlot(
      data: Array[Double],
      kMin: Int = 1,
      kMax: Int = -1,
      step: Int = 1
  ): PickandsPlotResult =
    require(data.length >= 5, "Data must have at least 5 observations")

    val sorted = data.clone()
    java.util.Arrays.sort(sorted)

    val n = sorted.length
    // Maximum valid k is floor((n-1)/4) since we need 4k < n
    val maxValidK = (n - 1) / 4
    val actualKMax = if kMax < 0 then maxValidK else math.min(kMax, maxValidK)
    val actualKMin = math.max(kMin, 1)

    require(
      actualKMin <= actualKMax,
      s"kMin ($actualKMin) must be <= kMax ($actualKMax), n=$n allows k up to $maxValidK"
    )

    // Calculate number of k values
    val numK = (actualKMax - actualKMin) / step + 1
    val kValues = new Array[Int](numK)
    val gammaEstimates = new Array[Double](numK)
    val alphaEstimates = new Array[Double](numK)

    var idx = 0
    var k = actualKMin
    while k <= actualKMax do
      val x_nk = sorted(n - k)
      val x_n2k = sorted(n - 2 * k)
      val x_n4k = sorted(n - 4 * k)

      val numerator = x_nk - x_n2k
      val denominator = x_n2k - x_n4k

      val gamma =
        if denominator > 0 && numerator > 0 then math.log(numerator / denominator) / ln2
        else Double.NaN

      kValues(idx) = k
      gammaEstimates(idx) = gamma
      alphaEstimates(idx) = if gamma > 0 then 1.0 / gamma else Double.NaN
      idx += 1
      k += step
    end while

    PickandsPlotResult(kValues, gammaEstimates, alphaEstimates)
  end pickandsPlot

end PickandsEstimator

object PickandsEstimatorExtensions:

  extension (vec: Array[Double])

    /** Computes the Pickands estimator for the extreme value index γ.
      *
      * @param k
      *   The tuning parameter (must satisfy 4k < n)
      * @return
      *   The estimated extreme value index γ
      */
    inline def pickandsEstimator(k: Int): Double = PickandsEstimator(vec, k)

    /** Computes the Pareto tail index α using the Pickands estimator.
      *
      * @param k
      *   The tuning parameter
      * @return
      *   The estimated tail index α = 1/γ
      */
    inline def pickandsTailIndex(k: Int): Double = PickandsEstimator.tailIndex(vec, k)

    /** Computes a Pickands plot for this data.
      *
      * @param kMin
      *   Minimum k value (default: 1)
      * @param kMax
      *   Maximum k value (default: (n-1)/4)
      * @param step
      *   Step size for k values (default: 1)
      * @return
      *   PickandsPlotResult with k values and estimates
      */
    inline def pickandsPlot(
        kMin: Int = 1,
        kMax: Int = -1,
        step: Int = 1
    ): PickandsEstimator.PickandsPlotResult =
      PickandsEstimator.pickandsPlot(vec, kMin, kMax, step)
  end extension

end PickandsEstimatorExtensions
