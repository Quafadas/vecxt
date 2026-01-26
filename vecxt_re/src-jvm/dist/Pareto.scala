package vecxt_re

import org.apache.commons.statistics.distribution.ParetoDistribution
import org.apache.commons.rng.simple.RandomSource
import io.github.quafadas.plots.SetupVega.{*, given}
import io.circe.syntax.*

/** Pareto Type I Distribution.
  *
  * The Pareto distribution is a power-law probability distribution commonly used to model the distribution of wealth,
  * insurance losses, and other phenomena where small values are common and large values are rare but possible.
  *
  * For scale parameter k (minimum possible value) and shape parameter α (Pareto index):
  *   - PDF: f(x) = α * k^α / x^(α+1) for x >= k
  *   - CDF: F(x) = 1 - (k/x)^α for x >= k
  *   - Mean: k * α / (α - 1) for α > 1, otherwise infinite
  *   - Variance: k² * α / ((α-1)² * (α-2)) for α > 2, otherwise infinite
  *
  * @param scale
  *   Scale parameter k (minimum possible value of X, must be positive)
  * @param shape
  *   Shape parameter α (Pareto index, must be positive)
  */
case class Pareto(scale: Double, shape: Double)
    extends ContinuousDistr[Double]
    with HasMean[Double]
    with HasVariance[Double]
    with HasCdf
    with HasInverseCdf:

  require(scale > 0, "scale must be positive")
  require(shape > 0, "shape must be positive")

  private val rng = RandomSource.XO_RO_SHI_RO_128_PP.create()
  private val distribution = ParetoDistribution.of(scale, shape)
  private val sampler = distribution.createSampler(rng)

  /** Draw a random sample from the Pareto distribution */
  def draw: Double = sampler.sample()

  /** Unnormalized log PDF */
  def unnormalizedLogPdf(x: Double): Double =
    if x < scale then Double.NegativeInfinity
    else distribution.logDensity(x)

  /** Log normalizer (Pareto is already normalized, so this is 0) */
  def logNormalizer: Double = 0.0

  /** Probability that x < X <= y */
  def probability(x: Double, y: Double): Double = distribution.probability(x, y)

  /** Cumulative distribution function */
  def cdf(x: Double): Double = distribution.cumulativeProbability(x)

  /** Inverse CDF (quantile function) */
  def inverseCdf(p: Double): Double = distribution.inverseCumulativeProbability(p)

  /** Survival function P(X > x) */
  def survivalProbability(x: Double): Double = distribution.survivalProbability(x)

  /** Inverse survival probability */
  def inverseSurvivalProbability(p: Double): Double = distribution.inverseSurvivalProbability(p)

  def mean: Double = distribution.getMean()

  def variance: Double = distribution.getVariance()

  private def guessMaxXForPlot = shape match
      case s if s > 2 => mean + 4 * math.sqrt(variance) // mean and variance are defined
      case s if s > 1 => mean + 20 * scale // no well defined variance
      case _          => scale * 10 // no well defined mean

  def plot(using viz.LowPriorityPlotTarget) =

    val linePlot = VegaPlot.fromResource("paretoPdf.vl.json")
    val maxX = guessMaxXForPlot
    val numPoints = 1000
    val data = (0 until numPoints).map { _ =>
      (x = draw)
    }
    linePlot.plot(
      _.layer.head.data.values := data.asJson,
      _.layer(1).data.sequence.start := scale,
      _.layer(1).data.sequence.stop := maxX,
      _.layer(1).data.sequence.step := (maxX - scale) / 200,
      _ += (title = s"Pareto Distribution PDF (scale=$scale, shape=$shape)").asJson
    )
  end plot

  def plotCdf(using viz.LowPriorityPlotTarget) =
    val linePlot = VegaPlot.fromResource("paretoCdf.vl.json")
    val maxX = guessMaxXForPlot

    linePlot.plot(
      _.data.sequence.start := scale,
      _.data.sequence.stop := maxX,
      _.data.sequence.step := (maxX - scale) / 200,
      _.transform.head.calculate := s"1 - pow($scale / datum.data, $shape)",
      _ += (title = s"Pareto Distribution CDF (scale=$scale, shape=$shape)").asJson
    )
  end plotCdf

end Pareto
