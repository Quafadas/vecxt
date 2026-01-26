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
    *   Named tuple with `dist`: the fitted NegativeBinomial distribution, and `converged`: whether the optimizer
    *   converged within maxIter
    */
  def mle(
      observations: Array[Int],
      maxIter: Int = 500,
      tol: Double = 1e-8
  ): (dist: NegativeBinomial, converged: Boolean) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.forall(_ >= 0), "all observations must be non-negative")

    val n = observations.length.toDouble
    val xbar = observations.sum / n
    val sumX = observations.sum.toDouble
    // Constant term in the log-likelihood: -∑ log Γ(x_i+1)
    var sumLogFact = 0.0
    var _i = 0
    while _i < observations.length do
      sumLogFact += LogGamma.value(observations(_i) + 1)
      _i += 1
    end while

    // Profile log-likelihood with b = xbar/a  (equivalently p = a/(a+xbar))
    def profileLogLik(a: Double): Double =
      if a <= 0 || !a.isFinite then Double.NegativeInfinity
      else
        val p = a / (a + xbar)
        val logP = math.log(p)
        val log1MinusP = math.log1p(-p)
        var ll = n * a * logP + sumX * log1MinusP - sumLogFact - n * LogGamma.value(a)
        var k = 0
        while k < observations.length do
          ll += LogGamma.value(a + observations(k))
          k += 1
        end while
        ll

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

        // Backtracking line search on the profile log-likelihood to improve robustness.
        val llCur = profileLogLik(a)
        var step = 1.0
        var aNew = a + step * delta
        var llNew = profileLogLik(aNew)
        while step > 1e-6 && llNew < llCur do
          step *= 0.5
          aNew = a + step * delta
          llNew = profileLogLik(aNew)
        end while

        if aNew <= 0 || !aNew.isFinite then a = a / 2.0
        else a = aNew
        end if

        converged = math.abs(step * delta) < tol * math.abs(a)
        iter += 1
      end while

      val bFinal = xbar / a
      (NegativeBinomial(a, bFinal), converged)
    end if
  end mle

  /** Maximum likelihood estimation for the volume-adjusted Negative Binomial.
    *
    * We observe pairs $(n_j, v_j)$ where $n_j$ is the count and $v_j$ is the volume ratio (historical volume / modeled
    * volume). With parameters $(r, \beta)$ and $p = 1/(1+\beta v_j)$ the likelihood is $$ L(r,\beta) = \prod_j
    * \frac{\Gamma(r+n_j)}{\Gamma(r)\,\Gamma(n_j+1)} \left(\frac{\beta v_j}{1+\beta v_j}\right)^{n_j}
    * \left(\frac{1}{1+\beta v_j}\right)^r. $$ The log-likelihood is $$ \ell(r,\beta) = \sum_j \big[\log\Gamma(r+n_j) -
    * \log\Gamma(r) - \log\Gamma(n_j+1) + n_j(\log(\beta v_j) - \log(1+\beta v_j)) - r\,\log(1+\beta v_j)\big]. $$
    * Gradient components: $$\partial_\beta \ell = \sum_j \Big( \frac{n_j}{\beta(1+\beta v_j)} - \frac{r v_j}{1+\beta
    * v_j} \Big),\quad \partial_r \ell = \sum_j \big[\psi(r+n_j) - \psi(r) - \log(1+\beta v_j)\big],$$ and Hessian
    * entries: $$\partial^2_{\beta\beta} \ell = \sum_j \Big( \frac{r v_j}{(1+\beta v_j)^2} - \frac{n_j(1+2\beta
    * v_j)}{\beta^2(1+\beta v_j)^2} \Big),$$ $$\partial^2_{rr} \ell = \sum_j \big[\psi'(r+n_j) - \psi'(r)\big],\quad
    * \partial^2_{\beta r} \ell = -\sum_j \frac{v_j}{1+\beta v_j}.$$
    *
    * Implementation details:
    *   - Initialize from method of moments on rates $n_j / v_j$; if underdispersed, start at a small $\beta$.
    *   - Newton updates solve the $2\times2$ system from the gradient/Hessian; a tiny ridge is added to keep the
    *     Hessian invertible.
    *   - Step halving is applied to enforce positivity of $r$ and $\beta$.
    *
    * @param observations
    *   non-negative counts $n_j$
    * @param volumes
    *   positive volume ratios $v_j$ (same units as modeled period)
    * @param maxIter
    *   maximum Newton steps
    * @param tol
    *   relative tolerance on both parameters
    * @return
    *   tuple of fitted `NegativeBinomial(r, beta)` and a convergence flag
    */
  def volweightedMle(
      observations: Array[Int],
      volumes: Array[Double],
      maxIter: Int = 500,
      tol: Double = 1e-8
  ): (dist: NegativeBinomial, converged: Boolean) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.length == volumes.length, "observations and volumes must have the same length")
    require(observations.forall(_ >= 0), "all observations must be non-negative")
    require(volumes.forall(v => v > 0 && v.isFinite), "volumes must be positive and finite")

    val nObs = observations.length

    var i = 0
    var sumRate = 0.0
    while i < nObs do
      sumRate += observations(i) / volumes(i)
      i += 1
    end while

    val meanRate = sumRate / nObs
    require(meanRate > 0, "mean per unit volume must be positive for NB fitting")

    var varRate = 0.0
    i = 0
    while i < nObs do
      val rate = observations(i) / volumes(i)
      val diff = rate - meanRate
      varRate += diff * diff
      i += 1
    end while
    varRate /= nObs.toDouble

    val betaFloor = 1e-6
    var beta =
      if varRate <= meanRate then betaFloor
      else math.max((varRate / meanRate) - 1.0, betaFloor)
    var r = meanRate / beta

    var iter = 0
    var converged = false
    val ridge = 1e-12

    while iter < maxIter && !converged do
      var gBeta = 0.0
      var gR = 0.0
      var hbb = 0.0
      var hrr = 0.0
      var hbr = 0.0

      i = 0
      while i < nObs do
        val n = observations(i).toDouble
        val v = volumes(i)
        val betaV = beta * v
        val denom = 1.0 + betaV
        val invDenom = 1.0 / denom
        val invDenom2 = invDenom * invDenom
        val invBeta = 1.0 / beta

        gBeta += n * invBeta * invDenom - r * v * invDenom
        gR += org.apache.commons.numbers.gamma.Digamma.value(r + n) -
          org.apache.commons.numbers.gamma.Digamma.value(r) -
          math.log(denom)

        hbb += r * v * invDenom2 - n * (1.0 + 2.0 * betaV) * invBeta * invBeta * invDenom2
        hrr += org.apache.commons.numbers.gamma.Trigamma.value(r + n) -
          org.apache.commons.numbers.gamma.Trigamma.value(r)
        hbr -= v * invDenom
        i += 1
      end while

      val hbbAdj = hbb + ridge
      val hrrAdj = hrr + ridge
      val det = hbbAdj * hrrAdj - hbr * hbr

      if det.isNaN || det.isInfinite || math.abs(det) < 1e-18 then iter = maxIter
      else
        val deltaBeta = (gBeta * hrrAdj - gR * hbr) / det
        val deltaR = (hbbAdj * gR - hbr * gBeta) / det

        var step = 1.0
        var newBeta = beta - step * deltaBeta
        var newR = r - step * deltaR

        while step > 1e-3 && (newBeta <= 0 || newR <= 0 || newBeta.isNaN || newR.isNaN) do
          step *= 0.5
          newBeta = beta - step * deltaBeta
          newR = r - step * deltaR
        end while

        if newBeta > 0 && newR > 0 && newBeta.isFinite && newR.isFinite then
          beta = newBeta
          r = newR
          converged = math.abs(step * deltaBeta) <= tol * math.abs(beta) &&
            math.abs(step * deltaR) <= tol * math.abs(r)
        else iter = maxIter
        end if
      end if

      iter += 1
    end while

    (NegativeBinomial(r, beta), converged)
  end volweightedMle

  inline def mleVolumeWeighted(
      observations: Array[Int],
      volumes: Array[Double],
      maxIter: Int = 100,
      tol: Double = 1e-8
  ): (dist: NegativeBinomial, converged: Boolean) = volweightedMle(observations, volumes, maxIter, tol)

end NegativeBinomial
