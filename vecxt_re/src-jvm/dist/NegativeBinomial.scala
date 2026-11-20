package vecxt_re

import org.apache.commons.numbers.gamma.LogBeta
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

  private val q: Double = b / (1.0 + b) // 1 − p, accurate for small b (unlike 1 − p from the rounded p)
  // From b directly, not via the rounded p: for small b, log(p) and log(1 − p) lose ~ε absolutely and relatively, and
  // a·log(p) then carries an error of a·ε (O(1e-6) at a = 1e10).
  private val logP: Double = -math.log1p(b)
  private val log1MinusP: Double = math.log(b) - math.log1p(b) // log(1-p) = log(b/(1+b))

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

  /** Log PMF: log P(X = k) = logΓ(a + k) − logΓ(a) − logΓ(k + 1) + a·log(p) + k·log(1−p).
    *
    * For k ≥ 1 the gamma terms equal −log k − log B(a, k). Differencing log Γ directly loses ~a·log(a)·ε absolutely,
    * which is garbage for large a (near-Poisson fits, [[NegativeBinomial.poisson]]); log B is evaluated without that
    * cancellation.
    */
  override def logProbabilityOf(x: Int): Double =
    if x < 0 then Double.NegativeInfinity
    else if x == 0 then a * logP
    else -math.log(x) - LogBeta.value(a, x) + a * logP + x * log1MinusP

  def mean: Double = a * b

  def variance: Double = a * b * (1.0 + b)

  override def probability(x: Int, y: Int): Double =
    if x >= y then 0.0
    else cdf(y) - cdf(x)

  override def cdf(x: Int): Double =
    if x < 0 then 0.0
    else
      // CDF of NegBin(a, p) at k = I_p(a, k+1) = 1 − I_q(k+1, a), q = 1 − p = b/(1+b), with I the regularized
      // incomplete beta function. Evaluated as the complement at q: for small b, p = 1/(1+b) rounds next to 1 and
      // I_p(a, ·) with large a (near-Poisson) was off by ~10% at a = 1e15.
      org.apache.commons.numbers.gamma.RegularizedBeta.complement(q, x.toDouble + 1.0, a)

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

  /** An NB fit is returned only if its log-likelihood beats Poisson by more than this fraction of Σᵢ (nᵢ·|log μᵢ| +
    * μᵢ), the magnitude of the terms whose cancellation sets the rounding noise in ℓ. With integer counts and equal
    * volumes the boundary score can be exactly zero, and rounding alone would otherwise yield an NB with r ~ 1e15.
    * Gains this small are statistically meaningless (the LR statistic is 2·gain).
    */
  private inline val MinRelativeLogLikGain = 1e-11

  /** Σᵢ (nᵢ·|log μᵢ| + μᵢ) with μᵢ = λ·vᵢ: the scale of the Poisson log-likelihood terms. */
  private def logLikScale(observations: Array[Int], volumes: Array[Double], lambda: Double): Double =
    var s = 0.0
    var i = 0
    while i < observations.length do
      val mu = lambda * volumes(i)
      s += observations(i) * math.abs(math.log(mu)) + mu
      i += 1
    end while
    s
  end logLikScale

  /** (1 + 1/y)·log(1 + y) − 1 for y ≥ 0, accurate for small y where the direct form cancels. */
  private def onePlusInvLog1pMinusOne(y: Double): Double =
    if y < 0.05 then
      // Σ_{j≥1} (−1)^(j+1) yʲ / (j·(j+1)), truncated at j = 13 (relative error < y¹³)
      var acc = 0.0
      var j = 13
      while j >= 1 do
        acc = (if j % 2 == 1 then 1.0 else -1.0) / (j * (j + 1)) + y * acc
        j -= 1
      end while
      y * acc
    else (1.0 + 1.0 / y) * math.log1p(y) - 1.0

  /** y − log(1 + y) for y ≥ 0, accurate for small y where the direct form cancels. */
  private def yMinusLog1p(y: Double): Double =
    if y < 0.1 then
      // y²·Σ_{j≥2} (−1)ʲ y^(j−2) / j, truncated at j = 19 (relative error < y¹⁸)
      var acc = 0.0
      var j = 19
      while j >= 2 do
        acc = (if j % 2 == 0 then 1.0 else -1.0) / j + y * acc
        j -= 1
      end while
      y * y * acc
    else y - math.log1p(y)

  /** Maximum likelihood estimation for Negative Binomial parameters.
    *
    * Use: the result is the distribution of a further observation (e.g. the forecast year), with variability calibrated
    * on history. It is a plug-in estimate: uncertainty in (a, b) itself is not included. For observations at differing
    * volumes, use [[volweightedMle]].
    *
    * Profiles out b (the MLE of the mean is x̄, so b = x̄/a) and solves the one-dimensional profile score equation S(a) =
    * 0 by safeguarded Newton-Raphson, starting from the method of moments estimate.
    *
    * {{{
    * Parameterisation p = 1/(1+b), mean = a·b, with b = x̄/a. Counts are integers, so
    * ψ(a + x) − ψ(a) = Σ_{k<x} 1/(a + k), and with cₖ = #{i : xᵢ > k}:
    *
    *   S(a)   = Σᵢ [ψ(a + xᵢ) − ψ(a)] − n·log(1 + x̄/a)
    *          = Σₖ cₖ / (a + k) − n·log(1 + x̄/a)                       (used when x̄/a ≥ 1)
    *          = n·(y − log(1 + y)) − (1/a)·Σₖ cₖ·k / (a + k),  y = x̄/a  (used when x̄/a < 1)
    *
    * The second form uses Σₖ cₖ = n·x̄ to cancel the leading n·x̄/a of both terms analytically; near the
    * Poisson boundary each term of the first form is ≈ n·x̄/a while S itself is O(1/a²).
    *
    *   −S′(a) = Σₖ cₖ / (a + k)² − n·x̄ / (a·(a + x̄))
    * }}}
    *
    * The finite sums replace differences of digamma / trigamma values, which cancel catastrophically near the Poisson
    * boundary (a → ∞): at a ≈ 2·10⁴ the special-function form loses ~4 significant digits of a, and even the first
    * finite-sum form loses ~1 digit to the cancellation above. The two terms of −S′ still nearly cancel for large a
    * (−S′ is O(1/a³) while each term is O(1/a²)), but each is now computed to full relative precision.
    *
    * When the population variance exceeds x̄ the profile score has a unique root, with S > 0 below it and S < 0 above
    * it (Levin & Reeds, 1977). Each evaluation of S therefore tightens a bracket [lo, hi] around the root. A Newton
    * step is taken when −S′ > 0 and the step stays inside the bracket; otherwise the iteration bisects (geometrically)
    * or, while the bracket is still open on one side, doubles / halves `a`.
    *
    * Convergence requires a Newton step (or the bracket width) smaller than `tol·a`. The step size is tested before the
    * bracket, so a step that rounds to nothing counts as converged rather than triggering a bisection.
    *
    * Cost per iteration is O(max xᵢ) after a single O(n) pass to build cₖ.
    *
    * The NB is returned only if it beats Poisson(x̄) by more than rounding noise in ℓ (see `MinRelativeLogLikGain`).
    * With integer counts the variance can equal x̄ exactly yet compute a few ulps above it, which would otherwise yield
    * an NB with a ~ 1e15.
    *
    * @param observations
    *   array of non-negative integer observations
    * @param maxIter
    *   maximum number of Newton-Raphson iterations
    * @param tol
    *   relative convergence tolerance for parameter 'a'
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
    require(maxIter > 0, "maxIter must be positive")
    require(tol > 0 && tol.isFinite, "tol must be positive and finite")

    val n = observations.length.toDouble
    val stats = observations.meanAndVariance
    val xbar = stats.mean
    val variance = stats.variance

    require(xbar > 0, "mean must be positive for NB fitting")

    // If variance <= mean, data is underdispersed relative to Poisson
    // In this case, return Poisson distribution
    if variance <= xbar then (Poisson(xbar), true)
    else
      // cₖ = #{i : xᵢ > k}, k = 0 until max xᵢ
      var maxX = 0
      var i = 0
      while i < observations.length do
        if observations(i) > maxX then maxX = observations(i)
        end if
        i += 1
      end while
      val exceed = new Array[Double](maxX)
      i = 0
      while i < observations.length do
        val x = observations(i)
        if x > 0 then exceed(x - 1) += 1.0
        end if
        i += 1
      end while
      var k = maxX - 2
      while k >= 0 do
        exceed(k) += exceed(k + 1)
        k -= 1
      end while

      // Method of moments initial estimate: b = variance/mean - 1, a = mean/b
      val bMom = (variance / xbar) - 1.0
      var a = xbar / bMom

      // Bracket on the root of S. S(lo) > 0 and S(hi) < 0; 0 and +∞ mean "not yet bounded".
      var lo = 0.0
      var hi = Double.PositiveInfinity
      var iter = 0
      var converged = false

      while iter < maxIter && !converged do
        var sum1 = 0.0 // Σₖ cₖ / (a + k)
        var sum2 = 0.0 // Σₖ cₖ / (a + k)²
        var sumK = 0.0 // Σₖ cₖ·k / (a + k)
        var kk = 0
        while kk < maxX do
          val inv = 1.0 / (a + kk)
          val cInv = exceed(kk) * inv
          sum1 += cInv
          sum2 += cInv * inv
          sumK += cInv * kk
          kk += 1
        end while

        val y = xbar / a
        val score =
          if y < 1.0 then n * yMinusLog1p(y) - sumK / a // leading n·x̄/a cancelled analytically
          else sum1 - n * math.log1p(y)
        val negDeriv = sum2 - n * xbar / (a * (a + xbar))

        if score == 0.0 then converged = true
        else
          if score > 0 then lo = a else hi = a
          end if

          val step = score / negDeriv
          if negDeriv > 0 && math.abs(step) <= tol * a then
            a += step
            converged = true
          else
            val newton = a + step
            val newtonOk = negDeriv > 0 && newton.isFinite && newton > lo && newton < hi

            val aNext =
              if newtonOk then newton
              else if hi.isInfinite then 2.0 * lo
              else if lo == 0.0 then 0.5 * hi
              else math.sqrt(lo * hi)

            converged = lo > 0 && hi.isFinite && (hi - lo) <= tol * aNext
            a = aNext
          end if
        end if

        iter += 1
      end while

      // Likelihood gain over Poisson(x̄) at α = 1/a, y = x̄/a (the MLE of the mean is x̄ for both):
      //   ℓ_NB − ℓ_Poisson = Σₖ cₖ·log(1 + k/a) − n·x̄·[(1 + 1/y)·log(1 + y) − 1]
      var sumLog = 0.0
      var kk = 1
      while kk < maxX do
        sumLog += exceed(kk) * math.log1p(kk / a)
        kk += 1
      end while
      val gain = sumLog - n * xbar * onePlusInvLog1pMinusOne(xbar / a)
      val minGain = MinRelativeLogLikGain * n * xbar * (math.abs(math.log(xbar)) + 1.0)

      if gain <= minGain then (Poisson(xbar), converged)
      else (NegativeBinomial(a, xbar / a), converged)
      end if
    end if
  end mle

  /** Maximum likelihood estimation for the volume-adjusted Negative Binomial.
    *
    * We observe pairs (nᵢ, vᵢ) where nᵢ is the count and vᵢ is the volume ratio (historical volume / modelled volume).
    *
    * Model: each observation has its own claim intensity Λᵢ per unit of modelled volume, drawn independently from a
    * common Gamma(r, scale β), and nᵢ | Λᵢ ~ Poisson(Λᵢ·vᵢ). Hence the nᵢ are independent with nᵢ ~ NB(r, β·vᵢ), E[nᵢ] =
    * r·β·vᵢ and variance/mean = 1 + β·vᵢ: NB2 with offset log vᵢ, not the "independent units" model NB(r·vᵢ, β) whose
    * variance/mean is constant in volume. (A single Λ shared by all observations would make the nᵢ dependent and needs
    * a different likelihood.) The fitted NegativeBinomial(r, β) is the distribution at the modelled volume (v = 1).
    *
    * Use: the result is the forecast-year count distribution at the modelled volume, i.e. next year is a fresh draw Λ ~
    * Gamma(r, β), with year-to-year variability calibrated on history. It is a plug-in estimate: uncertainty in (r, β)
    * itself is not included.
    *
    * {{{
    * Parameterisation: rate λ = r·β (expected count per unit volume) and dispersion α = 1/r ≥ 0,
    * so β = λ·α and μᵢ = λ·vᵢ. α = 0 is exactly Poisson(λ).
    *
    * Log-likelihood, dropping −Σ log nᵢ!:
    *
    *   ℓ(λ, α) = Σᵢ [ Σ_{k<nᵢ} log(1 + k·α) + nᵢ·log μᵢ − (nᵢ + 1/α)·log(1 + α·μᵢ) ]
    *
    * This is Σᵢ [log Γ(r + nᵢ) − log Γ(r) + nᵢ·log(β·vᵢ) − (nᵢ + r)·log(1 + β·vᵢ)] with the large
    * terms cancelled analytically, so it stays accurate as α → 0 (r → ∞), where it tends to the
    * Poisson log-likelihood Σᵢ [nᵢ·log μᵢ − μᵢ].
    *
    * For fixed α, ℓ is strictly concave in η = log λ:
    *
    *   ∂ℓ/∂η   =  Σᵢ (nᵢ − μᵢ) / (1 + α·μᵢ)
    *   ∂²ℓ/∂η² = −Σᵢ μᵢ·(1 + α·nᵢ) / (1 + α·μᵢ)²   < 0
    *
    * so λ̂(α) is unique. The profile ℓₚ(α) = ℓ(λ̂(α), α) has derivative (envelope theorem)
    *
    *   ℓₚ′(α) = Σᵢ [ Σ_{k<nᵢ} k / (1 + k·α) − nᵢ·μᵢ / (1 + α·μᵢ)
    *                 + (log(1 + α·μᵢ) − α·μᵢ / (1 + α·μᵢ)) / α² ]
    *
    * At the Poisson boundary, with λ̂₀ = Σnᵢ / Σvᵢ:
    *
    *   ℓₚ′(0) = ½·Σᵢ [(nᵢ − λ̂₀·vᵢ)² − nᵢ]        (the NB2 score for overdispersion)
    * }}}
    *
    * Implementation details:
    *   - ℓₚ need not be unimodal when volumes differ. For n = (3, 12), v = (0.2, 5.2), ℓₚ′(0) < 0, so Poisson is a
    *     local maximum, yet the global maximum is an NB with r ≈ 2.04, β ≈ 2.87. The sign of ℓₚ′(0) is therefore not
    *     used as a gate. (With equal volumes the maximum is unique and ℓₚ′(0) > 0 ⇔ NB; Levin & Reeds, 1977.)
    *   - ℓₚ and ℓₚ′ are evaluated at α = 0 and on a log-spaced grid α ∈ [1e-8, 1e3] (8 points per decade). λ̂(α) is
    *     found by bracketed Newton on η, warm-started from the previous grid point.
    *   - Every grid interval where ℓₚ′ changes sign from + to − contains a local maximum; its root is refined by
    *     Illinois regula falsi to relative tolerance `tol` in α.
    *   - The best of Poisson (α = 0) and all refined local maxima is returned. A local maximum narrower than one grid
    *     spacing could in principle be missed. An NB is returned only if it beats Poisson by more than rounding noise
    *     (see `MinRelativeLogLikGain`); otherwise an exactly-zero boundary score could yield r ~ 1e15.
    *   - `localMaxima` counts the local maxima of ℓₚ on [0, ∞) seen on the grid: α = 0 if ℓₚ′(0) ≤ 0, each interior + →
    *     − sign change, and the upper edge if ℓₚ is still increasing there. More than one means the chosen mode can
    *     flip on small data changes and may imply a very different rate: such fits deserve a second look.
    *   - No special functions are needed: the k-sums use the counts cₖ = #{i : nᵢ > k}, so one profile evaluation costs
    *     O(number of observations + max nᵢ).
    *   - `converged = false` if any one-dimensional solve hits `maxIter`, or if ℓₚ is still increasing at α = 1e3 (r <
    *     1e-3, i.e. degenerate data such as extreme zero inflation), whether or not the edge is the best point found:
    *     beyond the grid ℓₚ may rise past any interior maximum. The best point found is still returned.
    *
    * @param observations
    *   non-negative integer counts nᵢ
    * @param volumes
    *   positive volume ratios vᵢ (historical volume / modelled volume)
    * @param maxIter
    *   maximum iterations for each one-dimensional solve (Newton for λ̂(α), root refinement for α)
    * @param tol
    *   relative tolerance on α, hence on r and β
    * @return
    *   named tuple: `dist`, the fitted distribution (Poisson if no NB improves the likelihood, otherwise
    *   NegativeBinomial); `converged`, the convergence flag; `localMaxima`, the number of local maxima of the profile
    *   likelihood (≥ 1; > 1 flags a multimodal fit)
    */
  def volweightedMle(
      observations: Array[Int],
      volumes: Array[Double],
      maxIter: Int = 500,
      tol: Double = 1e-8
  ): (dist: Poisson | NegativeBinomial, converged: Boolean, localMaxima: Int) =
    require(observations.nonEmpty, "observations must not be empty")
    require(observations.length == volumes.length, "observations and volumes must have the same length")
    require(observations.forall(_ >= 0), "all observations must be non-negative")
    require(volumes.forall(v => v > 0 && v.isFinite), "volumes must be positive and finite")
    require(maxIter > 0, "maxIter must be positive")
    require(tol > 0 && tol.isFinite, "tol must be positive and finite")

    val nObs = observations.length
    var sumN = 0L
    var maxN = 0
    var maxRate = 0.0
    var i = 0
    while i < nObs do
      val n = observations(i)
      sumN += n
      if n > maxN then maxN = n
      end if
      val rate = n / volumes(i)
      if rate > maxRate then maxRate = rate
      end if
      i += 1
    end while

    val lambda0 = sumN.toDouble / volumes.sumSIMD
    require(lambda0 > 0, "mean per unit volume must be positive for NB fitting")

    // cₖ = #{i : nᵢ > k}, k = 0 until maxN
    val exceed = new Array[Double](maxN)
    i = 0
    while i < nObs do
      val n = observations(i)
      if n > 0 then exceed(n - 1) += 1.0
      end if
      i += 1
    end while
    var k = maxN - 2
    while k >= 0 do
      exceed(k) += exceed(k + 1)
      k -= 1
    end while

    var allConverged = true

    // λ̂(α) as η = log λ. ∂ℓ/∂η is decreasing in η, positive at etaLo (→ Σn) and ≤ 0 at etaHi (λ = max nᵢ/vᵢ).
    val etaHi = math.log(maxRate)
    val etaLo = etaHi - 60.0
    def solveEta(alpha: Double, start: Double): Double =
      var lo = etaLo
      var hi = etaHi
      var eta = math.min(math.max(start, lo), hi)
      var iter = 0
      var done = false
      while iter < maxIter && !done do
        val lam = math.exp(eta)
        var g = 0.0
        var h = 0.0
        var j = 0
        while j < nObs do
          val n = observations(j).toDouble
          val mu = lam * volumes(j)
          val invD = 1.0 / (1.0 + alpha * mu)
          g += (n - mu) * invD
          h += mu * (1.0 + alpha * n) * invD * invD
          j += 1
        end while
        if g == 0.0 then done = true
        else
          if g > 0 then lo = eta else hi = eta
          end if
          val step = g / h
          // Test the Newton step before the bracket: near the root eta + step can round to eta itself, which the
          // strict bracket test below would reject.
          if math.abs(step) <= 1e-10 * math.max(1.0, math.abs(eta)) then
            eta += step
            done = true
          else
            val newton = eta + step
            eta = if newton > lo && newton < hi then newton else 0.5 * (lo + hi)
          end if
        end if
        iter += 1
      end while
      if !done then allConverged = false
      end if
      eta
    end solveEta

    // (log(1 + x) − x/(1 + x)) / x², by series for small x to avoid cancellation.
    inline def fOverX2(x: Double): Double =
      if x < 0.05 then
        // Σ_{j≥2} (−1)ʲ (j − 1)/j · x^(j−2), truncated at j = 14 (error < x¹²)
        var acc = 0.0
        var j = 14
        while j >= 2 do
          val c = (j - 1).toDouble / j
          acc = (if j % 2 == 0 then c else -c) + x * acc
          j -= 1
        end while
        acc
      else (math.log1p(x) - x / (1.0 + x)) / (x * x)

    // ℓₚ and ℓₚ′ at (α, η = log λ̂(α)); written to profLl / profDeriv.
    var profLl = 0.0
    var profDeriv = 0.0
    def profile(alpha: Double, eta: Double): Unit =
      val lam = math.exp(eta)
      var ll = 0.0
      var dl = 0.0
      var kk = 1
      while kk < maxN do
        val ka = kk * alpha
        ll += exceed(kk) * math.log1p(ka)
        dl += exceed(kk) * kk / (1.0 + ka)
        kk += 1
      end while
      var j = 0
      while j < nObs do
        val n = observations(j).toDouble
        val mu = lam * volumes(j)
        val x = alpha * mu
        val l1p = math.log1p(x)
        val l1pOverX = if x == 0.0 then 1.0 else l1p / x // (1/α)·log(1 + αμ) = μ·l1pOverX
        ll += n * math.log(mu) - n * l1p - mu * l1pOverX
        dl += -n * mu / (1.0 + x) + mu * mu * fOverX2(x)
        j += 1
      end while
      profLl = ll
      profDeriv = dl
    end profile

    // Grid: α = 0, then 10^(−8 + j/8) for j = 0..88.
    val perDecade = 8
    val nGrid = 1 + 11 * perDecade + 1
    val alphas = new Array[Double](nGrid)
    val etas = new Array[Double](nGrid)
    val lls = new Array[Double](nGrid)
    val derivs = new Array[Double](nGrid)
    alphas(0) = 0.0
    etas(0) = math.log(lambda0)
    profile(0.0, etas(0))
    lls(0) = profLl
    derivs(0) = profDeriv
    var g = 1
    while g < nGrid do
      alphas(g) = math.pow(10.0, -8.0 + (g - 1).toDouble / perDecade)
      etas(g) = solveEta(alphas(g), etas(g - 1))
      profile(alphas(g), etas(g))
      lls(g) = profLl
      derivs(g) = profDeriv
      g += 1
    end while

    var bestAlpha = 0.0
    var bestEta = etas(0)
    var bestLl = lls(0)
    var localMaxima = if derivs(0) <= 0 then 1 else 0

    // Refine each local maximum: ℓₚ′ goes from + to − across [alphas(g), alphas(g+1)].
    g = 0
    while g < nGrid - 1 do
      if derivs(g) > 0 && derivs(g + 1) <= 0 then
        localMaxima += 1
        var a = alphas(g)
        var fa = derivs(g)
        var b = alphas(g + 1)
        var fb = derivs(g + 1)
        var eta = etas(g + 1)
        var c = b
        var cEta = eta
        var iter = 0
        var done = fb == 0.0
        var side = 0
        while iter < maxIter && !done do
          c = (a * fb - b * fa) / (fb - fa)
          if !(c > a && c < b) then c = 0.5 * (a + b)
          end if
          eta = solveEta(c, eta)
          profile(c, eta)
          val fc = profDeriv
          cEta = eta
          if fc == 0.0 then done = true
          else if fc > 0 then
            a = c
            fa = fc
            if side == -1 then fb *= 0.5 // Illinois: halve the stale end
            end if
            side = -1
          else
            b = c
            fb = fc
            if side == 1 then fa *= 0.5
            end if
            side = 1
          end if
          if b - a <= tol * b then done = true
          end if
          iter += 1
        end while
        if !done then allConverged = false
        end if
        cEta = solveEta(c, cEta)
        profile(c, cEta)
        if profLl > bestLl then
          bestLl = profLl
          bestAlpha = c
          bestEta = cEta
        end if
      end if
      g += 1
    end while

    // Still increasing at the largest α: a maximum lies beyond the grid (r < 1e-3), and ℓₚ may rise past any interior
    // maximum out there, so the result is unreliable whichever point wins.
    if derivs(nGrid - 1) > 0 then
      localMaxima += 1
      allConverged = false
      if lls(nGrid - 1) > bestLl then
        bestLl = lls(nGrid - 1)
        bestAlpha = alphas(nGrid - 1)
        bestEta = etas(nGrid - 1)
      end if
    end if

    val minGain = MinRelativeLogLikGain * logLikScale(observations, volumes, lambda0)
    if bestAlpha == 0.0 || bestLl - lls(0) <= minGain then (Poisson(lambda0), allConverged, localMaxima)
    else
      val lambda = math.exp(bestEta)
      (NegativeBinomial(1.0 / bestAlpha, lambda * bestAlpha), allConverged, localMaxima)
    end if
  end volweightedMle

  inline def mleVolumeWeighted(
      observations: Array[Int],
      volumes: Array[Double],
      maxIter: Int = 100,
      tol: Double = 1e-8
  ): (dist: Poisson | NegativeBinomial, converged: Boolean, localMaxima: Int) =
    volweightedMle(observations, volumes, maxIter, tol)

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
