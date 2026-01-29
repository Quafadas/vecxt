package vecxt_re

import org.apache.commons.numbers.gamma.LogGamma
import org.apache.commons.rng.simple.RandomSource
import org.apache.commons.statistics.distribution.PoissonDistribution

import io.circe.syntax.*
import vecxt.all.*
import io.github.quafadas.plots.SetupVega.{*, given}

/** Poisson Distribution.
  *
  * The Poisson distribution models the number of events occurring in a fixed interval of time or space, given that
  * these events occur with a known constant mean rate and independently of the time since the last event.
  *
  * Parameterization:
  *   - λ (lambda) = mean = variance
  *
  * PMF: P(X = k) = λ^k * e^(-λ) / k!
  *
  * The Poisson distribution is a limiting case of the Negative Binomial distribution as the dispersion parameter b →
  * 0.
  *
  * @param lambda
  *   the rate parameter (must be positive)
  */
case class Poisson(lambda: Double)
    extends DiscreteDistr[Int]
    with HasMean[Double]
    with HasCdf[Int]
    with HasVariance[Double]:
  require(lambda > 0, "lambda must be positive")
  require(lambda.isFinite, "lambda must be finite")

  private val logLambda: Double = math.log(lambda)

  private val rng = RandomSource.XO_RO_SHI_RO_128_PP.create()
  private val poissonDistribution = PoissonDistribution.of(lambda)
  private val poissonSampler = poissonDistribution.createSampler(rng)

  /** Draw a sample from the Poisson distribution */
  inline def draw: Int = poissonSampler.sample()

  /** PMF: P(X = k) = λ^k * e^(-λ) / k! */
  def probabilityOf(x: Int): Double =
    if x < 0 then 0.0
    else math.exp(logProbabilityOf(x))

  /** Log PMF: log P(X = k) = k*log(λ) - λ - log(k!) */
  override def logProbabilityOf(x: Int): Double =
    if x < 0 then Double.NegativeInfinity
    else x * logLambda - lambda - LogGamma.value(x + 1)

  inline def mean: Double = lambda

  inline def variance: Double = lambda

  override def probability(x: Int, y: Int): Double =
    if x >= y then 0.0
    else cdf(y) - cdf(x)

  override def cdf(x: Int): Double =
    if x < 0 then 0.0
    else
      // CDF using regularized incomplete gamma function
      // P(X <= k) = Q(k+1, λ) = Γ(k+1, λ) / Γ(k+1)
      // which is the upper regularized gamma function
      org.apache.commons.numbers.gamma.RegularizedGamma.Q.value(x.toDouble + 1.0, lambda)

  def plot(using viz.LowPriorityPlotTarget) =
    val linePlot = VegaPlot.fromResource("poissonProb.vl.json")
    val maxX = (mean + 4 * math.sqrt(variance)).toInt
    val data = (0 to maxX).map { k =>
      (value = k, prob = probabilityOf(k))
    }
    linePlot.plot(
      _.data.values := data.asJson,
      _ += (title = s"Poisson Distribution Marginal Probabilities (λ=$lambda)").asJson
    )
  end plot

  def plotCdf(using viz.LowPriorityPlotTarget) =
    val linePlot = VegaPlot.fromResource("poissonCumul.vl.json")
    val maxX = (mean + 4 * math.sqrt(variance)).toInt
    var cumProb = 0.0
    val data = (0 to maxX).map { k =>
      cumProb += probabilityOf(k)
      (value = k, prob = cumProb)
    }
    linePlot.plot(
      _.data.values := data.asJson,
      _ += (title = s"Poisson Distribution Cumulative Probabilities (λ=$lambda)").asJson
    )
  end plotCdf
end Poisson

object Poisson:
  /** Create a Poisson distribution from the mean.
    *
    * @param mu
    *   the mean (rate) parameter
    * @return
    *   a Poisson distribution with the given mean
    */
  inline def fromMean(mu: Double): Poisson = Poisson(mu)

  /** Maximum likelihood estimation for Poisson parameter.
    *
    * For Poisson, the MLE of λ is simply the sample mean. This is exact and always converges in one step.
    *
    * @param observations
    *   array of non-negative integer observations
    * @return
    *   Named tuple with `dist`: the fitted Poisson distribution, and `converged`: always true for Poisson MLE
    */
  def mle(observations: Array[Int]): (dist: Poisson, converged: Boolean) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.forall(_ >= 0), "all observations must be non-negative")

    val lambdaHat = observations.mean
    require(lambdaHat > 0, "mean must be positive for Poisson fitting")

    (Poisson(lambdaHat), true)
  end mle

  /** Maximum likelihood estimation for volume-adjusted Poisson.
    *
    * For observations $n_j$ with corresponding volumes $v_j$, the Poisson model assumes $n_j \sim
    * \text{Poisson}(\lambda v_j)$.
    *
    * The MLE for $\lambda$ is: $$ \hat{\lambda} = \frac{\sum_j n_j}{\sum_j v_j} $$
    *
    * @param observations
    *   non-negative counts $n_j$
    * @param volumes
    *   positive volume ratios $v_j$ (same units as modeled period)
    * @return
    *   tuple of fitted `Poisson(lambda)` and a convergence flag (always true for Poisson)
    */
  def volweightedMle(
      observations: Array[Int],
      volumes: Array[Double]
  ): (dist: Poisson, converged: Boolean) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.length == volumes.length, "observations and volumes must have the same length")
    require(observations.forall(_ >= 0), "all observations must be non-negative")
    require(volumes.forall(v => v > 0 && v.isFinite), "volumes must be positive and finite")

    val sumN = observations.sumSIMD.toDouble
    val sumV = volumes.sum
    val lambdaHat = sumN / sumV

    require(lambdaHat > 0, "rate must be positive for Poisson fitting")

    (Poisson(lambdaHat), true)
  end volweightedMle

  inline def mleVolumeWeighted(
      observations: Array[Int],
      volumes: Array[Double]
  ): (dist: Poisson, converged: Boolean) = volweightedMle(observations, volumes)

  /** Perform a chi-squared goodness-of-fit test to assess whether the observed data follows a Poisson distribution.
    *
    * Groups observations into bins and computes the chi-squared statistic comparing observed to expected frequencies.
    *
    * @param observations
    *   array of non-negative integer observations
    * @param lambda
    *   the Poisson rate parameter (if None, uses MLE from data)
    * @param minExpected
    *   minimum expected frequency per bin (bins are combined to meet this threshold)
    * @return
    *   Named tuple with `statistic`: the chi-squared test statistic, `degreesOfFreedom`: the degrees of freedom, and
    *   `pValue`: the p-value of the test
    */
  def goodnessOfFit(
      observations: Array[Int],
      lambda: Option[Double] = None,
      minExpected: Double = 5.0
  ): (statistic: Double, degreesOfFreedom: Int, pValue: Double) =
    require(observations.nonEmpty, "observations must not be empty")
    require(minExpected > 0, "minExpected must be positive")

    val n = observations.length.toDouble
    val lambdaEst = lambda.getOrElse(observations.sumSIMD.toDouble / n)
    val poisson = Poisson(lambdaEst)

    // Find the max observation to determine bin range
    var maxObs = observations.maxSIMD


    // Count observations in each bin
    val counts = new Array[Int](maxObs + 2) // +1 for the "maxObs or more" bin
    var i = 0
    while i < observations.length do
      val obs = observations(i)
      if obs >= counts.length - 1 then counts(counts.length - 1) += 1
      else counts(obs) += 1
      i += 1
    end while

    // Compute expected frequencies
    val expected = new Array[Double](counts.length)
    i = 0
    while i < expected.length - 1 do
      expected(i) = n * poisson.probabilityOf(i)
      i += 1
    end while
    // Last bin is "maxObs or more"
    expected(expected.length - 1) = n * (1.0 - poisson.cdf(expected.length - 2))

    // Combine bins with expected < minExpected
    var chiSq = 0.0
    var df = -1 // Start at -1 because we estimated lambda
    var obsAccum = 0
    var expAccum = 0.0

    i = 0
    while i < counts.length do
      obsAccum += counts(i)
      expAccum += expected(i)
      if expAccum >= minExpected then
        chiSq += (obsAccum - expAccum) * (obsAccum - expAccum) / expAccum
        df += 1
        obsAccum = 0
        expAccum = 0.0
      end if
      i += 1
    end while

    // Handle remaining accumulated values
    if expAccum > 0 then
      // Add to previous bin's chi-squared contribution
      chiSq += (obsAccum - expAccum) * (obsAccum - expAccum) / expAccum
      df += 1
    end if

    // Compute p-value using chi-squared distribution
    val pValue =
      if df <= 0 then 1.0
      else 1.0 - org.apache.commons.numbers.gamma.RegularizedGamma.P.value(df.toDouble / 2.0, chiSq / 2.0)

    (chiSq, df, pValue)
  end goodnessOfFit

end Poisson
