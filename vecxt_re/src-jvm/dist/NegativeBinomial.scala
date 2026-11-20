package vecxt_re

import org.apache.commons.numbers.gamma.LogGamma
import org.apache.commons.rng.simple.RandomSource
import org.apache.commons.statistics.distribution.GammaDistribution
import org.apache.commons.statistics.distribution.PoissonDistribution

import vecxt.all.*

import io.circe.syntax.*
import io.github.quafadas.plots.SetupVega.{*, given}

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
    with HasCdf[Int]
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

  override def probability(x: Int, y: Int): Double =
    if x >= y then 0.0
    else cdf(y) - cdf(x)

  override def cdf(x: Int): Double =
    if x < 0 then 0.0
    else
      // CDF of NegBin(r, p) at k = I_p(r, k+1)
      // where I_p(a, b) is the regularized incomplete beta function
      // For our parameterization: p = 1/(1+b) is the success probability
      org.apache.commons.numbers.gamma.RegularizedBeta.value(p, a, x.toDouble + 1.0)

  def plot(using viz.LowPriorityPlotTarget): viz.VizReturn = plot(Nil)

  /** As [[plot]], applying `mods` (e.g. [[Plots.fixedSize]]) to the spec after the built-in modifications. */
  def plot(mods: Seq[Plots.SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
    val linePlot = VegaPlot.fromResource("negBinProb.vl.json")
    val maxX = (mean + 4 * math.sqrt(variance)).toInt
    val data = (0 to maxX).map { k =>
      (value = k, prob = probabilityOf(k))
    }
    Plots.render(linePlot, mods)(
      _.data.values := data.asJson,
      _ += (title = s"Negative Binomial Distribution Marginal Probabilities (a=$a, b=$b)").asJson
    )
  end plot

  def plotCdf(using viz.LowPriorityPlotTarget): viz.VizReturn = plotCdf(Nil)

  /** As [[plotCdf]], applying `mods` (e.g. [[Plots.fixedSize]]) to the spec after the built-in modifications. */
  def plotCdf(mods: Seq[Plots.SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
    val linePlot = VegaPlot.fromResource("negBinCumul.vl.json")
    val maxX = (mean + 4 * math.sqrt(variance)).toInt
    var cumProb = 0.0
    val data = (0 to maxX).map { k =>
      cumProb += probabilityOf(k)
      (value = k, prob = cumProb)
    }
    Plots.render(linePlot, mods)(
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
    * Profiles out b (the MLE of the mean is x̄, so b = x̄/a) and solves the one-dimensional profile score equation S(a) =
    * 0 by safeguarded Newton-Raphson, starting from the method of moments estimate.
    *
    * For parameterization p = 1/(1+b), mean = a*b, with b = x̄/a:
    *   - Score: S(a) = Σᵢ [ψ(a + xᵢ) - ψ(a)] + n·log(a/(a + x̄))
    *   - Derivative: S'(a) = Σᵢ [ψ'(a + xᵢ) - ψ'(a)] + n·x̄/(a·(a + x̄))
    *
    * The two terms of S'(a) nearly cancel for large a (S' is O(1/a³) while each term is O(1/a²)), so S' must be
    * computed with the correct sign or Newton steps become far too small.
    *
    * When the population variance exceeds x̄ the profile score has a unique root, with S > 0 below it and S < 0 above
    * it (Levin & Reeds, 1977). Each evaluation of S therefore tightens a bracket [lo, hi] around the root. A Newton
    * step is taken when -S' > 0 and the step stays inside the bracket; otherwise the iteration bisects (geometrically)
    * or, while the bracket is still open on one side, doubles / halves `a`.
    *
    * Convergence requires a Newton step (or the bracket width) smaller than `tol·a`. Because S' is correct, a Newton
    * step estimates the distance to the root, so a small step means the score is genuinely near zero.
    *
    * @param observations
    *   array of non-negative integer observations
    * @param maxIter
    *   maximum number of Newton-Raphson iterations
    * @param tol
    *   convergence tolerance for parameter 'a'
    * @return
    *   Named tuple with `dist`: the fitted distribution (Poisson if no overdispersion, otherwise NegativeBinomial), and
    *   `converged`: whether the optimizer converged within maxIter
    */
  def mle(
      observations: Array[Int],
      maxIter: Int = 500,
      tol: Double = 1e-8
  ): (dist: Poisson | NegativeBinomial, converged: Boolean) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.forall(_ >= 0), "all observations must be non-negative")

    val n = observations.length.toDouble
    val stats = observations.meanAndVariance
    val xbar = stats.mean
    val variance = stats.variance

    require(xbar > 0, "mean must be positive for NB fitting")

    // If variance <= mean, data is underdispersed relative to Poisson
    // In this case, return Poisson distribution
    if variance <= xbar then (Poisson(xbar), true)
    else
      // Method of moments initial estimate: b = variance/mean - 1, a = mean/b
      val bMom = (variance / xbar) - 1.0
      var a = xbar / bMom

      // Bracket on the root of S. S(lo) > 0 and S(hi) < 0; 0 and +∞ mean "not yet bounded".
      var lo = 0.0
      var hi = Double.PositiveInfinity
      var iter = 0
      var converged = false

      while iter < maxIter && !converged do
        // S(a) = Σᵢ ψ(a + xᵢ) - n·ψ(a) - n·log1p(x̄/a)
        // -S'(a) = n·ψ'(a) - Σᵢ ψ'(a + xᵢ) - n·x̄/(a·(a + x̄))
        var sumDigamma = 0.0
        var sumTrigamma = 0.0
        var i = 0
        while i < observations.length do
          val ax = a + observations(i)
          sumDigamma += org.apache.commons.numbers.gamma.Digamma.value(ax)
          sumTrigamma += org.apache.commons.numbers.gamma.Trigamma.value(ax)
          i += 1
        end while

        val score = sumDigamma - n * org.apache.commons.numbers.gamma.Digamma.value(a) - n * math.log1p(xbar / a)
        val negDeriv =
          n * org.apache.commons.numbers.gamma.Trigamma.value(a) - sumTrigamma - n * xbar / (a * (a + xbar))

        if score == 0.0 then converged = true
        else
          if score > 0 then lo = a else hi = a
          end if

          val newton = a + score / negDeriv
          val newtonOk = negDeriv > 0 && newton.isFinite && newton > lo && newton < hi

          val aNext =
            if newtonOk then newton
            else if hi.isInfinite then 2.0 * lo
            else if lo == 0.0 then 0.5 * hi
            else math.sqrt(lo * hi)

          val bracketClosed = lo > 0 && hi.isFinite && (hi - lo) <= tol * aNext
          converged = (newtonOk && math.abs(aNext - a) <= tol * aNext) || bracketClosed
          a = aNext
        end if

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
    *   tuple of fitted distribution (Poisson if no overdispersion, otherwise NegativeBinomial) and a convergence flag
    */
  def volweightedMle(
      observations: Array[Int],
      volumes: Array[Double],
      maxIter: Int = 500,
      tol: Double = 1e-8
  ): (dist: Poisson | NegativeBinomial, converged: Boolean) =
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

    // If variance <= mean, data is underdispersed relative to Poisson
    // Return Poisson distribution with rate = sum(n) / sum(v)
    if varRate <= meanRate then
      val sumN = observations.sumSIMD.toDouble
      val sumV = volumes.sum
      return (Poisson(sumN / sumV), true)
    end if

    val betaFloor = 1e-6
    var beta = math.max((varRate / meanRate) - 1.0, betaFloor)
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
  ): (dist: Poisson | NegativeBinomial, converged: Boolean) = volweightedMle(observations, volumes, maxIter, tol)

  /** JSON codec. Only `a` and `b` are written; derived state (sampler, RNG) is rebuilt on decode. Constructor
    * validation failures surface as a [[io.circe.DecodingFailure]].
    */
  given negativeBinomialEncoder: io.circe.Encoder[NegativeBinomial] =
    io.circe.Encoder.forProduct2[NegativeBinomial, Double, Double]("a", "b")(nb => (nb.a, nb.b))

  given negativeBinomialDecoder: io.circe.Decoder[NegativeBinomial] =
    io.circe.Decoder
      .forProduct2[(Double, Double), Double, Double]("a", "b")((_, _))
      .emapTry((a, b) => scala.util.Try(NegativeBinomial(a, b)))

end NegativeBinomial
