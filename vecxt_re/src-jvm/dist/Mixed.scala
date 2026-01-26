package vecxt_re

import org.apache.commons.rng.simple.RandomSource
import io.github.quafadas.plots.SetupVega.{*, given}
import io.circe.syntax.*

/** Mixed distribution: Empirical body with Pareto tail.
  *
  * This distribution combines an empirical distribution for the body (values below the mixing point) with a Pareto
  * distribution for the tail (values at or above the mixing point).
  *
  * The distribution is parameterized by:
  *   - An empirical distribution of observed values
  *   - A mixing point $m$ (threshold between body and tail)
  *   - A Pareto shape parameter $\alpha$ (for the tail)
  *
  * The CDF is continuous at the mixing point. Let $p_m = F_{\text{emp}}(m^-)$ be the empirical CDF just below the
  * mixing point. Then:
  *   - For $x < m$: $F(x) = F_{\text{emp}}(x)$
  *   - For $x \ge m$: $F(x) = p_m + (1 - p_m) \cdot F_{\text{Pareto}}(x)$
  *
  * where the Pareto distribution has scale = $m$ and shape = $\alpha$.
  *
  * @param empirical
  *   The empirical distribution for the body
  * @param mixingPoint
  *   The threshold where we switch from empirical to Pareto tail
  * @param paretoShape
  *   The shape parameter (α) for the Pareto tail
  */
case class Mixed(empirical: Empirical, mixingPoint: Double, paretoShape: Double)
    extends ContinuousDistr[Double]
    with HasMean[Double]
    with HasVariance[Double]
    with HasCdf
    with HasInverseCdf:

  require(mixingPoint > 0, "mixing point must be positive")
  require(paretoShape > 0, "Pareto shape must be positive")

  private val rng = RandomSource.XO_RO_SHI_RO_128_PP.create()

  // The Pareto tail with scale = mixing point
  private val paretoTail = Pareto(mixingPoint, paretoShape)

  // Probability mass in the empirical body (CDF at the mixing point)
  // We want P(X < mixingPoint) from the empirical, which is the sum of all mass points strictly below mixingPoint
  private val bodyWeight: Double =
    // The empirical CDF is right-continuous, so cdf(m) includes P(X <= m).
    // We want mass strictly below m for the body, and P(X >= m) goes to the tail.
    // However, for simplicity and continuity, we use cdf(m-epsilon) conceptually.
    // In practice, we compute the probability of all empirical points strictly below the mixing point.
    var w = 0.0
    val vals = empirical.values
    val weights = empirical.weights
    var totalW = 0.0
    var i = 0
    while i < vals.length do
      totalW += weights(i)
      if vals(i) < mixingPoint then w += weights(i)
      i += 1
    end while
    w / totalW
  end bodyWeight

  // Tail weight is the complement
  private val tailWeight: Double = 1.0 - bodyWeight

  /** Draw a random sample from the mixed distribution */
  def draw: Double =
    val u = rng.nextDouble()
    inverseCdf(u)
  end draw

  /** Unnormalized log PDF.
    *
    * For the body (empirical), this is technically undefined in the continuous sense since the empirical distribution
    * is discrete. We return the log of the weighted probability mass if x exactly matches an empirical point, otherwise
    * negative infinity.
    *
    * For the tail (Pareto), we return the properly weighted log PDF.
    */
  def unnormalizedLogPdf(x: Double): Double =
    if x < mixingPoint then
      // Discrete mass in the body
      val prob = empirical.probabilityOf(x)
      if prob > 0 then math.log(prob) else Double.NegativeInfinity
    else
      // Continuous Pareto tail, scaled by tail weight
      if tailWeight > 0 then paretoTail.unnormalizedLogPdf(x) + math.log(tailWeight)
      else Double.NegativeInfinity
    end if
  end unnormalizedLogPdf

  /** Log normalizer (distribution is already normalized) */
  def logNormalizer: Double = 0.0

  /** Probability that x < X <= y */
  def probability(x: Double, y: Double): Double =
    if y <= x then 0.0
    else cdf(y) - cdf(x)

  /** Cumulative distribution function.
    *
    * For x < mixingPoint: F(x) = bodyWeight * (empirical CDF normalized to body)
    * For x >= mixingPoint: F(x) = bodyWeight + tailWeight * F_Pareto(x)
    */
  def cdf(x: Double): Double =
    if x < mixingPoint then
      // Use empirical CDF, but only count points below mixing point
      // The CDF here is P(X <= x) for X in the body region, scaled by bodyWeight
      val empCdfAtX = empirical.cdf(x)
      // Scale: empirical CDF goes up to 1, but we only want it to contribute bodyWeight
      math.min(empCdfAtX, bodyWeight) // Cap at bodyWeight since empirical points >= mixingPoint don't count
    else
      // In the tail region
      bodyWeight + tailWeight * paretoTail.cdf(x)
    end if
  end cdf

  /** Inverse CDF (quantile function) */
  def inverseCdf(p: Double): Double =
    require(p >= 0.0 && p <= 1.0, "p must be in [0,1]")
    if p <= 0.0 then
      // Return minimum of empirical or mixing point
      empirical.inverseCdf(0.0)
    else if p <= bodyWeight then
      // In the body region - use empirical inverse CDF
      // Scale p to [0, 1] within the body
      val scaledP = p / bodyWeight
      val q = empirical.inverseCdf(math.min(scaledP, 1.0))
      // Ensure we don't exceed mixing point
      math.min(q, mixingPoint - Double.MinPositiveValue)
    else
      // In the tail region - use Pareto inverse CDF
      val tailP = (p - bodyWeight) / tailWeight
      paretoTail.inverseCdf(tailP)
    end if
  end inverseCdf

  /** Mean of the mixed distribution.
    *
    * E[X] = bodyWeight * E[X | X < m] + tailWeight * E[X_Pareto]
    *
    * Note: For Pareto, mean is only defined when shape > 1.
    */
  def mean: Double =
    // Compute conditional mean of empirical given X < mixingPoint
    var empMean = 0.0
    var empWeight = 0.0
    val vals = empirical.values
    val weights = empirical.weights
    var totalW = 0.0
    var i = 0
    while i < vals.length do
      totalW += weights(i)
      i += 1
    end while
    i = 0
    while i < vals.length do
      if vals(i) < mixingPoint then
        val w = weights(i) / totalW
        empMean += vals(i) * w
        empWeight += w
      end if
      i += 1
    end while
    val condEmpMean = if empWeight > 0 then empMean / empWeight else 0.0

    bodyWeight * condEmpMean + tailWeight * paretoTail.mean
  end mean

  /** Variance of the mixed distribution.
    *
    * Uses the law of total variance.
    *
    * Note: For Pareto, variance is only defined when shape > 2.
    */
  def variance: Double =
    val m = mean
    // Compute E[X^2] for the body
    var empSecondMoment = 0.0
    var empWeight = 0.0
    val vals = empirical.values
    val weights = empirical.weights
    var totalW = 0.0
    var i = 0
    while i < vals.length do
      totalW += weights(i)
      i += 1
    end while
    i = 0
    while i < vals.length do
      if vals(i) < mixingPoint then
        val w = weights(i) / totalW
        empSecondMoment += vals(i) * vals(i) * w
        empWeight += w
      end if
      i += 1
    end while
    val condEmpSecondMoment = if empWeight > 0 then empSecondMoment / empWeight else 0.0

    // E[X^2] for Pareto
    val paretoSecondMoment = paretoTail.variance + paretoTail.mean * paretoTail.mean

    // Total E[X^2]
    val totalSecondMoment = bodyWeight * condEmpSecondMoment + tailWeight * paretoSecondMoment

    // Var(X) = E[X^2] - E[X]^2
    totalSecondMoment - m * m
  end variance

  /** Plot the mixed distribution PDF/histogram. */
  def plot(using viz.LowPriorityPlotTarget) =
    val plot = VegaPlot.fromResource("mixedPdf.vl.json")
    val numSamples = 10000
    val samples = (0 until numSamples).map(_ => (x = draw))

    plot.plot(
      _.data.values := samples.asJson,
      _ += (title = s"Mixed Distribution (mixingPoint=$mixingPoint, paretoShape=$paretoShape)").asJson
    )
  end plot

  /** Plot the mixed CDF. */
  def plotCdf(using viz.LowPriorityPlotTarget) =
    val plot = VegaPlot.fromResource("mixedCdf.vl.json")

    // Generate points for the CDF
    val minX = empirical.inverseCdf(0.0)
    val maxX = paretoTail.inverseCdf(0.99) // 99th percentile of tail
    val numPoints = 500
    val step = (maxX - minX) / numPoints

    val points = (0 to numPoints).map { i =>
      val x = minX + i * step
      (x = x, cdf = cdf(x))
    }

    plot.plot(
      _.data.values := points.asJson,
      _ += (title = s"Mixed Distribution CDF (mixingPoint=$mixingPoint, paretoShape=$paretoShape)").asJson
    )
  end plotCdf

end Mixed

object Mixed:

  /** Create a mixed distribution from raw empirical data.
    *
    * @param values
    *   The empirical sample values
    * @param mixingPoint
    *   The threshold between body and tail
    * @param paretoShape
    *   The Pareto shape parameter for the tail
    */
  inline def fromValues(values: Array[Double], mixingPoint: Double, paretoShape: Double): Mixed =
    Mixed(Empirical.equalWeights(values), mixingPoint, paretoShape)

  /** Create a mixed distribution from weighted empirical data.
    *
    * @param values
    *   The empirical sample values
    * @param weights
    *   The weights for each sample value
    * @param mixingPoint
    *   The threshold between body and tail
    * @param paretoShape
    *   The Pareto shape parameter for the tail
    */
  inline def fromWeightedValues(
      values: Array[Double],
      weights: Array[Double],
      mixingPoint: Double,
      paretoShape: Double
  ): Mixed =
    Mixed(Empirical.weighted(values, weights), mixingPoint, paretoShape)

end Mixed
