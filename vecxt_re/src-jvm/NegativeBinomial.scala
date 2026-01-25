package vecxt_re

import org.apache.commons.numbers.gamma.LogGamma
import org.apache.commons.statistics.distribution.GammaDistribution
import org.apache.commons.statistics.distribution.PoissonDistribution
import org.apache.commons.rng.simple.RandomSource
import io.github.quafadas.plots.SetupVega.{*, given}
import io.circe.syntax.*

/** Negative Binomial Distribution with alternative parameterization.
  *
  * Uses the parameterization:
  *   - r = a (number of successes, can be any positive real)
  *   - p = 1 / (1 + b) (probability of success)
  *
  * Which gives:
  *   - mean = a * b
  *   - variance = a * b * (1 + b)
  *
  * Under this parameterisation, as b -> 0, the distribution will converge to Poisson(ab). The parameter b is therefore
  * a measure of overdispersion.
  *
  * Implementation uses the gamma-Poisson mixture representation, which allows non-integer a: If λ ~ Gamma(a, b) and X |
  * λ ~ Poisson(λ), then X ~ NegativeBinomial(a, b)
  *
  * @param a
  *   shape parameter (must be positive, can be non-integer)
  * @param b
  *   scale/dispersion parameter (must be positive)
  */

//TODO: JS, facade to Stdlib gamma, poisson etc.
case class NegativeBinomial(a: Double, b: Double)
    extends DiscreteDistr[Int]
    with HasMean[Double]
    with HasVariance[Double]:
  require(a > 0, "a must be positive")
  require(b > 0, "b must be positive")
  require(a.isFinite, "a must be finite")
  require(b.isFinite, "b must be finite")

  private val p: Double = 1.0 / (1.0 + b)
  private val logP: Double = math.log(p)
  private val log1MinusP: Double = math.log1p(-p) // log(1-p) = log(b/(1+b))

  private val rng = RandomSource.XO_RO_SHI_RO_128_PP.create()

  // Gamma distribution with shape=a, scale=b for the mixture representation
  private val gammaDistribution = GammaDistribution.of(a, b)
  private val gammaSampler = gammaDistribution.createSampler(rng)

  /** Draw using gamma-Poisson mixture: λ ~ Gamma(a, b), X | λ ~ Poisson(λ) */
  def draw: Int =
    val lambda = gammaSampler.sample()
    if lambda <= 0 then 0
    else PoissonDistribution.of(lambda).createSampler(rng).sample()
    end if
  end draw

  /** PMF: P(X = k) = Γ(a + k) / (Γ(a) * k!) * p^a * (1-p)^k
    */
  def probabilityOf(x: Int): Double =
    if x < 0 then 0.0
    else math.exp(logProbabilityOf(x))

  /** Log PMF: log P(X = k) = logΓ(a + k) - logΓ(a) - logΓ(k + 1) + a*log(p) + k*log(1-p)
    */
  override def logProbabilityOf(x: Int): Double =
    if x < 0 then Double.NegativeInfinity
    else
      LogGamma.value(a + x) - LogGamma.value(a) - LogGamma.value(x + 1) +
        a * logP + x * log1MinusP

  def mean: Double = a * b

  def variance: Double = a * b * (1.0 + b)

  def plot(using viz.LowPriorityPlotTarget) =
    val linePlot = VegaPlot.fromResource("negBinProb.vl.json")
    val maxX = (mean + 4 * math.sqrt(variance)).toInt
    val data = (0 to maxX).map { k =>
      (value = k, prob = probabilityOf(k))
    }
    linePlot.plot(
      _.data.values := data.asJson,
      _ += (title = s"Negative Binomial Distribution Marginal Probabilities (a=$a, b=$b)").asJson
    )
  end plot

  def plotCdf(using viz.LowPriorityPlotTarget) =
    val linePlot = VegaPlot.fromResource("negBinCumul.vl.json")
    val maxX = (mean + 4 * math.sqrt(variance)).toInt
    var cumProb = 0.0
    val data = (0 to maxX).map { k =>
      cumProb += probabilityOf(k)
      (value = k, prob = cumProb)
    }
    linePlot.plot(
      _.data.values := data.asJson,
      _ += (title = s"Negative Binomial Distribution Cumulative Probabilities (a=$a, b=$b)").asJson
    )
  end plotCdf
end NegativeBinomial

object NegativeBinomial:
  inline def fromMeanDispersion(mu: Double, b: Double): NegativeBinomial =
    NegativeBinomial(mu / b, b)

  inline def poisson(mu: Double): NegativeBinomial =
    NegativeBinomial(mu / 1e-12, 1e-12)

  /** Maximum likelihood estimation for Negative Binomial parameters.
    *
    * Uses Newton-Raphson iteration on the profile likelihood for 'a', with method of moments as the initial estimate.
    *
    * For parameterization p = 1/(1+b), mean = a*b, with b = mean/a:
    *   - Score: S(a) = Σᵢ [ψ(a + xᵢ) - ψ(a)] + n·log(a/(a + x̄))
    *   - Hessian: H(a) = Σᵢ [ψ'(a + xᵢ) - ψ'(a)] + n·x̄/(a·(a + x̄))
    *
    * @param observations
    *   array of non-negative integer observations
    * @param maxIter
    *   maximum number of Newton-Raphson iterations
    * @param tol
    *   convergence tolerance for parameter 'a'
    * @return
    *   Named tuple with `dist`: the fitted NegativeBinomial distribution, and `converged`: whether the optimizer converged within maxIter
    */
  def mle(observations: Array[Int], maxIter: Int = 100, tol: Double = 1e-8): (dist: NegativeBinomial, converged: Boolean) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.forall(_ >= 0), "all observations must be non-negative")

    val n = observations.length.toDouble
    val xbar = observations.sum / n
    val variance = observations.map(x => (x - xbar) * (x - xbar)).sum / n

    require(xbar > 0, "mean must be positive for NB fitting")

    // If variance <= mean, data is underdispersed relative to Poisson
    // In this case, return near-Poisson (small b)
    if variance <= xbar then (NegativeBinomial(xbar / 1e-10, 1e-10), true)
    else
      // Method of moments initial estimates:
      // b = variance/mean - 1
      // a = mean/b = mean^2 / (variance - mean)
      val bMom = (variance / xbar) - 1.0
      val aMom = xbar / bMom

      // Newton-Raphson iteration on the profile score equation for 'a'
      // With b = xbar/a, the profile log-likelihood score is:
      // S(a) = Σᵢ [ψ(a + xᵢ) - ψ(a)] + n·log(a/(a + xbar))
      var a = aMom
      var iter = 0
      var converged = false

      while iter < maxIter && !converged do
        // Score: S(a) = Σᵢ [ψ(a + xᵢ) - ψ(a)] + n·log(a/(a + xbar))
        var score = n * math.log(a / (a + xbar))

        // Hessian (negative): -H(a) = Σᵢ [ψ'(a) - ψ'(a + xᵢ)] + n·xbar/(a·(a + xbar))
        var negHessian = n * xbar / (a * (a + xbar))

        var i = 0
        while i < observations.length do
          val x = observations(i)
          score += org.apache.commons.numbers.gamma.Digamma.value(a + x) -
            org.apache.commons.numbers.gamma.Digamma.value(a)
          negHessian += org.apache.commons.numbers.gamma.Trigamma.value(a) -
            org.apache.commons.numbers.gamma.Trigamma.value(a + x)
          i += 1
        end while

        val delta = score / negHessian
        val aNew = a + delta

        if aNew <= 0 then a = a / 2.0
        else a = aNew

        converged = math.abs(delta) < tol * math.abs(a)
        iter += 1
      end while

      val bFinal = xbar / a
      (NegativeBinomial(a, bFinal), converged)
    end if
  end mle
end NegativeBinomial
