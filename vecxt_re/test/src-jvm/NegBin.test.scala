package vecxt_re

import munit.FunSuite
import vecxt.all.*
import org.apache.commons.statistics.distribution.PoissonDistribution

class NegBinTest extends FunSuite:

  inline val localTests = false

  test("pmf approximately normalizes") {
    val nb = NegativeBinomial(a = 2.5, b = 1.2)

    val mu = nb.mean
    val sd = math.sqrt(nb.variance)
    val K = (mu + 15 * sd).toInt

    val sum = (0 to K).map(nb.probabilityOf).sum

    assert(math.abs(sum - 1.0) < 1e-8)
  }

  test("pmf mean and variance match theory") {
    val nb = NegativeBinomial(3.0, 0.7)

    val K = 500
    val probs = (0 to K).map(k => nb.probabilityOf(k))

    val mean = probs.zipWithIndex.map { case (p, k) => p * k }.sum
    val varr = probs.zipWithIndex.map { case (p, k) => p * k * k }.sum - mean * mean

    assert(math.abs(mean - nb.mean) < 1e-6)
    assert(math.abs(varr - nb.variance) < 1e-6)
  }

  test("approaches Poisson as b -> 0") {
    val mu = 4.0
    val b = 1e-6
    val a = mu / b

    val nb = NegativeBinomial(a, b)
    val pois = PoissonDistribution.of(mu)

    assert(nb.probabilityOf(-1) == 0.0)
    assert(nb.logProbabilityOf(-1).isNegInfinity)

    (0 to 20).foreach { k =>
      val diff =
        math.abs(nb.probabilityOf(k) - pois.probability(k))
      assert(diff < 1e-6)
    }
  }

  test("works with small a < 1 (fractional shape)") {
    val nb = NegativeBinomial(a = 0.5, b = 2.0)

    // Verify PMF normalizes
    val K = 200
    val sum = (0 to K).map(nb.probabilityOf).sum
    assert(math.abs(sum - 1.0) < 1e-6)

    // Verify mean and variance from PMF match theoretical values
    val probs = (0 to K).map(k => nb.probabilityOf(k))
    val mean = probs.zipWithIndex.map { case (p, k) => p * k }.sum
    val varr = probs.zipWithIndex.map { case (p, k) => p * k * k }.sum - mean * mean

    // a * b = 0.5 * 2.0 = 1.0
    assert(math.abs(mean - nb.mean) < 1e-5)
    assert(math.abs(nb.mean - 1.0) < 1e-10)

    // a * b * (1 + b) = 0.5 * 2.0 * 3.0 = 3.0
    assert(math.abs(varr - nb.variance) < 1e-4)
    assert(math.abs(nb.variance - 3.0) < 1e-10)
  }

  // Ignored in CI as slow
  test("SLOW: sampling mean and variance") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.NegBinTest.sampling mean and variance IN CI========")
    val nb = NegativeBinomial(5.0, 0.8)
    val n = 2_000_000

    val xs = Array.fill(n)(nb.draw.toDouble)

    val mean = xs.sum / n
    val varr = xs.map(x => (x - mean) * (x - mean)).sum / n

    assert(math.abs(mean - nb.mean) < 5e-3)
    assert(math.abs(varr - nb.variance) < 5e-2)
  }

  test("SLOW: sampling distribution matches pmf") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.NegBinTest.sampling distribution matches pmf IN CI========")
    val nb = NegativeBinomial(2.0, 1.5)
    val n = 500_000

    val samples = Array.fill(n)(nb.draw)
    val counts = samples.groupBy(identity).view.mapValues(_.size).toMap

    val K = 20
    (0 to K).foreach { k =>
      val expected = n * nb.probabilityOf(k)
      val observed = counts.getOrElse(k, 0)
      assert(math.abs(observed - expected) < 5 * math.sqrt(expected))
    }
  }

  test("SLOW: MLE recovers parameters") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.MLE recovers parameters IN CI========")

    val trueNb = NegativeBinomial(4.0, 0.6)
    val data = Array.fill(10_000)(trueNb.draw)

    val (fitted, converged) = NegativeBinomial.mle(data)
    assert(converged)

    fitted match
      case nb: NegativeBinomial =>
        assertEqualsDouble(nb.mean, trueNb.mean, 0.1)
        assertEqualsDouble(nb.b, trueNb.b, 0.1)
      case _: Poisson =>
        fail("Expected NegativeBinomial but got Poisson")
    end match
  }

  test("SLOW: vol weighted MLE follows standard case with uniform volumes ") {

    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.MLE recovers parameters IN CI========")

    val trueNb = NegativeBinomial(4.0, 0.6)
    val data = Array.fill(10_000)(trueNb.draw)

    val (fitted, converged, _) = NegativeBinomial.mleVolumeWeighted(data, Array.fill(10_000)(1.0))
    assert(converged)

    fitted match
      case nb: NegativeBinomial =>
        assertEqualsDouble(nb.mean, trueNb.mean, 0.1)
        assertEqualsDouble(nb.b, trueNb.b, 0.1)
      case _: Poisson =>
        fail("Expected NegativeBinomial but got Poisson")
    end match
  }

  /** This directly exercises the volume factors: counts drawn with v = 0.5 use scale βv = 0.4, and with v = 2.0 use βv =
    * 1.6; the fitter must undo that scaling to recover β = 0.8.
    */
  test("SLOW: volume-weighted MLE recovers base params with mixed volumes") {
    assume(localTests, "Skip heavy sampling in CI")

    val rTrue = 3.2
    val betaTrue = 0.8
    val seed = 12345L
    val nPerBucket = 25_000
    val vols = Array.fill(nPerBucket)(0.5) ++ Array.fill(nPerBucket)(2.0)

    val rng = org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create(seed)
    val gammaLow = org.apache.commons.statistics.distribution.GammaDistribution
      .of(rTrue, betaTrue * 0.5)
      .createSampler(rng)
    val gammaHigh = org.apache.commons.statistics.distribution.GammaDistribution
      .of(rTrue, betaTrue * 2.0)
      .createSampler(rng)

    val data = new Array[Int](vols.length)
    var i = 0
    while i < vols.length do
      val lambda =
        if i < nPerBucket then gammaLow.sample()
        else gammaHigh.sample()
      data(i) = org.apache.commons.statistics.distribution.PoissonDistribution
        .of(lambda)
        .createSampler(rng)
        .sample()
      i += 1
    end while

    val (fitted, converged, _) = NegativeBinomial.mleVolumeWeighted(data, vols, maxIter = 200, tol = 1e-8)
    assert(converged)
    fitted match
      case nb: NegativeBinomial =>
        assertEqualsDouble(nb.a, rTrue, 0.1)
        assertEqualsDouble(nb.b, betaTrue, 0.1)

        // Ignoring volumes collapses a mixture of scaled NB's into a single NB, which should fit worse
        // (at minimum: it should be less accurate on the modeled-period mean and dispersion).
        val modeledMean = rTrue * betaTrue
        val (unweighted, _) = NegativeBinomial.mle(data)
        unweighted match
          case unNb: NegativeBinomial =>
            assert(math.abs(nb.mean - modeledMean) <= math.abs(unNb.mean - modeledMean))
            assert(math.abs(nb.b - betaTrue) <= math.abs(unNb.b - betaTrue))
          case _: Poisson =>
            // Unweighted returned Poisson, just check that the weighted NB is reasonable
            assert(math.abs(nb.mean - modeledMean) < 0.5)
        end match
      case _: Poisson =>
        fail("Expected NegativeBinomial but got Poisson for overdispersed data")
    end match
  }

  /** Seeded gamma-Poisson draws, so the sample (and hence its MLE) is fixed. */
  private def seededNb(a: Double, mean: Double, size: Int, seed: Long): Array[Int] =
    val rng = org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create(java.lang.Long.valueOf(seed))
    val gamma = org.apache.commons.statistics.distribution.GammaDistribution.of(a, mean / a).createSampler(rng)
    Array.fill(size) {
      val lambda = gamma.sample()
      if lambda <= 0 then 0 else PoissonDistribution.of(lambda).createSampler(rng).sample()
      end if
    }
  end seededNb

  /** Reference MLE for `a`: geometric bisection on the profile score, independent of the Newton iteration. */
  private def profileScoreRoot(x: Array[Int]): Double =
    import org.apache.commons.numbers.gamma.Digamma
    val n = x.length.toDouble
    val xbar = x.map(_.toDouble).sum / n
    def score(a: Double): Double =
      x.map(xi => Digamma.value(a + xi) - Digamma.value(a)).sum - n * math.log1p(xbar / a)
    var lo = 1e-6
    var hi = 1e6
    assert(score(lo) > 0 && score(hi) < 0, "root must be bracketed")
    var k = 0
    while k < 200 do
      val mid = math.sqrt(lo * hi)
      if score(mid) > 0 then lo = mid else hi = mid
      end if
      k += 1
    end while
    math.sqrt(lo * hi)
  end profileScoreRoot

  // Regression: S'(a) used to be computed with the wrong sign on the n·x̄/(a(a+x̄)) term, inflating the Newton
  // denominator by ~a×. Moderate-dispersion fits then crawled, hit maxIter unconverged, and stopped off the MLE.
  for (aTrue, mean, seed) <- Seq((5.0, 20.0, 11L), (20.0, 10.0, 12L), (50.0, 30.0, 13L), (0.7, 3.0, 14L)) do
    test(s"MLE matches profile-score root and converges quickly (a=$aTrue, mean=$mean)") {
      val data = seededNb(aTrue, mean, 500, seed)
      val expectedA = profileScoreRoot(data)
      val xbar = data.map(_.toDouble).sum / data.length

      // The correct Newton iteration converges quadratically; 25 iterations is ample.
      val (fitted, converged) = NegativeBinomial.mle(data, maxIter = 25)
      assert(converged, s"did not converge in 25 iterations (expected a=$expectedA)")

      fitted match
        case nb: NegativeBinomial =>
          assertEqualsDouble(nb.a, expectedA, 1e-6 * expectedA)
          assertEqualsDouble(nb.mean, xbar, 1e-9 * xbar)
        case _: Poisson =>
          fail(s"Expected NegativeBinomial but got Poisson (sample should be overdispersed)")
      end match
    }
  end for

  /** Seeded draws from the volume model: nᵢ ~ NB(r, β·vᵢ), i.e. Λ ~ Gamma(r, β·vᵢ), nᵢ | Λ ~ Poisson(Λ). */
  private def seededVolNb(
      r: Double,
      beta: Double,
      vols: Array[Double],
      rng: org.apache.commons.rng.UniformRandomProvider
  ): Array[Int] =
    vols.map { v =>
      val lambda =
        org.apache.commons.statistics.distribution.GammaDistribution.of(r, beta * v).createSampler(rng).sample()
      if lambda <= 0 then 0 else PoissonDistribution.of(lambda).createSampler(rng).sample()
      end if
    }

  private def seededRng(seed: Long) =
    org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create(java.lang.Long.valueOf(seed))

  /** Score equations (∂ℓ/∂r, ∂ℓ/∂β) from the docstring, scaled to (log r, log β) and per observation. */
  private def volScore(r: Double, beta: Double, data: Array[Int], vols: Array[Double]): (Double, Double) =
    import org.apache.commons.numbers.gamma.Digamma
    var gBeta = 0.0
    var gR = 0.0
    var i = 0
    while i < data.length do
      val n = data(i).toDouble
      val d = 1.0 + beta * vols(i)
      gBeta += n / (beta * d) - r * vols(i) / d
      gR += Digamma.value(r + n) - Digamma.value(r) - math.log(d)
      i += 1
    end while
    (r * gR / data.length, beta * gBeta / data.length)
  end volScore

  // Regression: ∂²ℓ/∂β² used r·v instead of r·v². Harmless when every v = 1, but with non-unit volumes the Newton
  // matrix was wrong; the small-volume case below diverged (β → 1e8, converged = false). The last case used to be
  // misclassified as Poisson by a volume-blind overdispersion gate.
  for (rTrue, betaTrue, vLo, vHi, seed) <- Seq(
      (4.0, 0.6, 0.5, 2.0, 21L),
      (4.0, 0.6, 0.2, 5.0, 22L),
      (2.0, 2.0, 0.1, 0.5, 23L),
      (10.0, 0.3, 1.0, 10.0, 24L)
    )
  do
    test(s"volume-weighted MLE zeroes the gradient quickly (r=$rTrue, β=$betaTrue, v∈[$vLo, $vHi])") {
      val rng = seededRng(seed)
      val vols = Array.fill(300)(vLo + (vHi - vLo) * rng.nextDouble())
      val data = seededVolNb(rTrue, betaTrue, vols, rng)

      val (fitted, converged, _) = NegativeBinomial.volweightedMle(data, vols, maxIter = 30)
      assert(converged, "did not converge in 30 iterations")

      fitted match
        case nb: NegativeBinomial =>
          val (gR, gBeta) = volScore(nb.a, nb.b, data, vols)
          assertEqualsDouble(gR, 0.0, 1e-8)
          assertEqualsDouble(gBeta, 0.0, 1e-8)
        case _: Poisson =>
          fail("Expected NegativeBinomial but got Poisson (sample should be overdispersed)")
      end match
    }
  end for

  // Regression: the gate compared Var(nᵢ/vᵢ) with its mean, which is volume-biased. With every v = 10, NB(4, 0.5) has
  // variance/mean = 6 for the counts, but the old gate returned Poisson whenever β ≤ 1 − 1/v.
  test("volume-weighted MLE detects overdispersion at large volumes") {
    val rng = seededRng(31L)
    val vols = Array.fill(2000)(10.0)
    val data = seededVolNb(4.0, 0.5, vols, rng)

    val (fitted, converged, _) = NegativeBinomial.volweightedMle(data, vols)
    assert(converged)
    fitted match
      case nb: NegativeBinomial =>
        assertEqualsDouble(nb.b, 0.5, 0.1)
        assertEqualsDouble(nb.mean, 2.0, 0.1) // r·β per unit modelled volume
      case _: Poisson =>
        fail("Expected NegativeBinomial: counts are strongly overdispersed")
    end match
  }

  // Regression: with every v = 0.1, Poisson rates look 10× overdispersed to the old gate, which then fitted an NB and
  // diverged to nonsense (r ≈ 0.02, β ≈ 1e8, converged = false). Under Poisson the score statistic is ~0, so either
  // outcome is legitimate, but an NB fit must be near-Poisson and reproduce λ̂.
  test("volume-weighted MLE on Poisson data at small volumes is Poisson or near-Poisson") {
    val rng = seededRng(32L)
    val vols = Array.fill(2000)(0.1)
    val poisson = PoissonDistribution.of(5.0 * 0.1).createSampler(rng)
    val data = Array.fill(vols.length)(poisson.sample())
    val lambdaHat = data.sum.toDouble / vols.sum

    val (fitted, converged, _) = NegativeBinomial.volweightedMle(data, vols)
    assert(converged)
    fitted match
      case p: Poisson =>
        assertEqualsDouble(p.lambda, lambdaHat, 1e-12)
      case nb: NegativeBinomial =>
        // Equal volumes, so the MLE of the mean is exactly λ̂.
        assertEqualsDouble(nb.mean, lambdaHat, 1e-6 * lambdaHat)
        assert(nb.b < 0.5, s"expected near-Poisson fit, got β=${nb.b}")
    end match
  }

  /** Full NB log-likelihood ℓ(r, β) (including −Σ log nᵢ!), naive form; fine for moderate r. */
  private def volNbLogLik(r: Double, beta: Double, data: Array[Int], vols: Array[Double]): Double =
    import org.apache.commons.numbers.gamma.LogGamma
    data.indices.map { i =>
      val n = data(i).toDouble
      val bv = beta * vols(i)
      LogGamma.value(r + n) - LogGamma.value(r) - LogGamma.value(n + 1) + n * (math.log(bv) - math.log1p(bv)) -
        r * math.log1p(bv)
    }.sum
  end volNbLogLik

  private def volPoissonLogLik(lambda: Double, data: Array[Int], vols: Array[Double]): Double =
    import org.apache.commons.numbers.gamma.LogGamma
    data.indices.map { i =>
      val n = data(i).toDouble
      val mu = lambda * vols(i)
      n * math.log(mu) - mu - LogGamma.value(n + 1)
    }.sum
  end volPoissonLogLik

  // Regression (external review): the score at the Poisson boundary is negative here, so Poisson is a local maximum,
  // but an interior NB has strictly higher likelihood. A gate on the sign of that score returned Poisson.
  test("volume-weighted MLE finds an interior NB maximum when the boundary score is negative") {
    val data = Array(3, 12)
    val vols = Array(0.2, 5.2)
    val lambdaHat = data.sum / vols.sum
    val tStat = data.indices.map(i => math.pow(data(i) - lambdaHat * vols(i), 2) - data(i)).sum
    assert(tStat < 0, s"precondition: boundary score should be negative, got $tStat")

    val (fitted, converged, localMaxima) = NegativeBinomial.volweightedMle(data, vols)
    assert(converged)
    assertEquals(localMaxima, 2, "Poisson boundary and the interior NB are both local maxima")
    fitted match
      case nb: NegativeBinomial =>
        assertEqualsDouble(nb.a, 2.04355, 1e-4)
        assertEqualsDouble(nb.b, 2.86871, 1e-4)
        val llNb = volNbLogLik(nb.a, nb.b, data, vols)
        val llPoisson = volPoissonLogLik(lambdaHat, data, vols)
        assertEqualsDouble(llNb, -6.29527, 1e-5)
        assert(llNb > llPoisson + 0.2, s"NB ℓ=$llNb should beat Poisson ℓ=$llPoisson")
        val (gR, gBeta) = volScore(nb.a, nb.b, data, vols)
        assertEqualsDouble(gR, 0.0, 1e-8)
        assertEqualsDouble(gBeta, 0.0, 1e-8)
      case _: Poisson =>
        fail("Expected NegativeBinomial: it has strictly higher likelihood than Poisson")
    end match
  }

  // With equal volumes the MLE is unique (Levin & Reeds), so the volume-weighted fit must agree with `mle`, an
  // independent implementation (profile in a with digamma/trigamma). The Poisson seeds give near-Poisson samples,
  // where either both return Poisson or both return an NB with large r — the regime where naive log Γ differences
  // lose precision.
  for (label, seed, sampler) <- Seq[(String, Long, org.apache.commons.rng.UniformRandomProvider => Int)](
      ("NB(5, 4)", 41L, rng => seededVolNb(5.0, 4.0, Array(1.0), rng)(0)),
      ("NB(30, 0.5)", 42L, rng => seededVolNb(30.0, 0.5, Array(1.0), rng)(0)),
      ("Poisson(40) a", 43L, rng => PoissonDistribution.of(40.0).createSampler(rng).sample()),
      ("Poisson(40) b", 44L, rng => PoissonDistribution.of(40.0).createSampler(rng).sample()),
      (
        "Poisson(40), slightly overdispersed sample, a ≈ 1e3",
        47L,
        rng => PoissonDistribution.of(40.0).createSampler(rng).sample()
      )
    )
  do
    test(s"volume-weighted MLE with unit volumes agrees with mle ($label)") {
      val rng = seededRng(seed)
      val data = Array.fill(400)(sampler(rng))
      val vols = Array.fill(data.length)(1.0)

      val (plain, plainConverged) = NegativeBinomial.mle(data)
      val (weighted, weightedConverged, localMaxima) = NegativeBinomial.volweightedMle(data, vols)
      assert(plainConverged && weightedConverged)
      assertEquals(localMaxima, 1, "equal volumes: the profile likelihood is unimodal")

      (plain, weighted) match
        case (p1: Poisson, p2: Poisson) =>
          assertEqualsDouble(p2.lambda, p1.lambda, 1e-12 * p1.lambda)
        case (nb1: NegativeBinomial, nb2: NegativeBinomial) =>
          assertEqualsDouble(nb2.a, nb1.a, 1e-6 * nb1.a)
          assertEqualsDouble(nb2.b, nb1.b, 1e-6 * nb1.b)
        case _ =>
          fail(s"mle returned $plain but volweightedMle returned $weighted")
      end match
    }
  end for

  // Near the Poisson boundary (a ≳ 1e4) differences of log Γ / digamma values cancel badly; before switching to exact
  // finite sums, `mle` was off by 6e-5 (seed 50) and 3e-4 (seed 73). Reference: root of the exact profile score
  // Σᵢ Σ_{k<xᵢ} 1/(a+k) − n·log(1 + x̄/a), solved by bisection in 60-digit decimal arithmetic.
  for (seed, xbar, expectedA) <- Seq((50L, 39.79, 1.505383619684580e4), (73L, 39.6875, 2.218324336138002e4)) do
    val rng = seededRng(seed)
    val data = Array.fill(400)(PoissonDistribution.of(40.0).createSampler(rng).sample())
    val vols = Array.fill(data.length)(1.0)
    for (fitter, fit) <- Seq[(String, () => (dist: Poisson | NegativeBinomial, converged: Boolean))](
        ("mle", () => NegativeBinomial.mle(data)),
        (
          "volweightedMle",
          () =>
            val r = NegativeBinomial.volweightedMle(data, vols)
            (dist = r.dist, converged = r.converged)
        )
      )
    do
      test(s"$fitter is accurate near the Poisson boundary (seed $seed, a ≈ ${expectedA.round})") {
        assertEqualsDouble(data.sum / 400.0, xbar, 0.0)
        val (fitted, converged) = fit()
        assert(converged)
        fitted match
          case nb: NegativeBinomial =>
            assertEqualsDouble(nb.a, expectedA, 1e-9 * expectedA)
            assertEqualsDouble(nb.mean, xbar, 1e-12 * xbar)
          case _: Poisson =>
            fail("Expected NegativeBinomial: the sample is (slightly) overdispersed")
        end match
      }
    end for
  end for

  // Regression (external review): with integer counts and equal volumes the boundary score Σ[(nᵢ − x̄)² − nᵢ] can be
  // exactly zero. It then computes a few ulps either side of zero, and when positive the fitters returned an NB with
  // r ~ 1e15–1e17 whose likelihood beat Poisson by rounding noise alone. Enumerate every such sample of 4 counts ≤ 8.
  test("exactly zero boundary score returns Poisson from both fitters") {
    val vols = Array.fill(4)(1.0)
    var cases = 0
    for
      a <- 0 to 8; b <- a to 8; c <- b to 8; d <- c to 8
      data = Array(a, b, c, d)
      s = data.sum
      if s > 0 && 4 * data.map(x => x * x).sum - s * s - 4 * s == 0 // N·Σn² − S² − N·S = 0 ⇔ score = 0
    do
      cases += 1
      for perm <- data.permutations do
        val (plain, _) = NegativeBinomial.mle(perm)
        val (weighted, _, localMaxima) = NegativeBinomial.volweightedMle(perm, vols)
        assert(plain.isInstanceOf[Poisson], s"mle(${perm.mkString(",")}) = $plain")
        assert(weighted.isInstanceOf[Poisson], s"volweightedMle(${perm.mkString(",")}) = $weighted")
        assertEquals(localMaxima, 1)
      end for
    end for
    assert(cases > 3, s"only $cases zero-score samples enumerated")
  }

  test("exactly zero boundary score: the review's example (4, 4, 0, 4)") {
    val (weighted, converged, localMaxima) = NegativeBinomial.volweightedMle(Array(4, 4, 0, 4), Array.fill(4)(1.0))
    assert(converged)
    assertEquals(localMaxima, 1)
    weighted match
      case p: Poisson           => assertEqualsDouble(p.lambda, 3.0, 1e-15)
      case nb: NegativeBinomial => fail(s"expected Poisson, got $nb")
    end match
  }

  // Regression (external review): `converged` was only cleared when the still-rising upper edge also beat every
  // interior maximum, contrary to the docstring. 2000 zeros and one large count put the MLE at r ≈ 5e-5, beyond the
  // grid's r = 1e-3.
  test("volume-weighted MLE flags non-convergence when the profile still rises at the grid edge") {
    val data = Array.fill(2000)(0) :+ 500
    val vols = Array.fill(data.length)(1.0)
    val (fitted, converged, localMaxima) = NegativeBinomial.volweightedMle(data, vols)
    assert(!converged, "the maximum lies beyond the grid")
    assert(localMaxima >= 1)
    fitted match
      case nb: NegativeBinomial => assertEqualsDouble(nb.a, 1e-3, 1e-12) // best point found: the edge
      case p: Poisson           => fail(s"expected the edge NB, got $p")
    end match
  }

  // log Γ(a + k) − log Γ(a) loses ~a·log(a)·ε absolutely; for a = 1e15 that is O(10) in the log PMF.
  test("pmf is accurate for very large a (near-Poisson)") {
    val lambda = 5.0
    val poisson = PoissonDistribution.of(lambda)
    for a <- Seq(1e6, 1e10, 1e15) do
      val nb = NegativeBinomial(a, lambda / a)
      for k <- 0 to 30 do
        val expected = poisson.logProbability(k)
        // NB(a, λ/a) differs from Poisson(λ) by O(k²/a) in log PMF
        val tol = 1e-12 * math.abs(expected) + (k * k + lambda * lambda) / a
        assertEqualsDouble(nb.logProbabilityOf(k), expected, tol, s"a=$a, k=$k")
      end for
    end for
    val viaPoisson = NegativeBinomial.poisson(lambda)
    assertEqualsDouble(viaPoisson.probabilityOf(3), poisson.probability(3), 1e-9 * poisson.probability(3))
  }

  // cdf used I_p(a, k+1) with p = 1/(1+b) rounded next to 1: 4e-7 relative error at a = 1e10, 10% at a = 1e15.
  test("cdf is accurate for very large a (near-Poisson)") {
    val lambda = 5.0
    val poisson = PoissonDistribution.of(lambda)
    for a <- Seq(1e6, 1e10, 1e15); k <- 0 to 25 do
      val expected = poisson.cumulativeProbability(k)
      val tol = 1e-12 + 2.0 * (k * k + lambda * lambda) / a * expected
      assertEqualsDouble(NegativeBinomial(a, lambda / a).cdf(k), expected, tol, s"a=$a, k=$k")
    end for
  }

  test("cdf equals the summed pmf for moderate a") {
    for (a, b) <- Seq((0.3, 4.0), (2.5, 1.2), (40.0, 0.05)) do
      val nb = NegativeBinomial(a, b)
      var acc = 0.0
      for k <- 0 to 60 do
        acc += nb.probabilityOf(k)
        assertEqualsDouble(nb.cdf(k), acc, 1e-12, s"a=$a, b=$b, k=$k")
      end for
    end for
  }

  test("pmf matches the log Γ form for moderate a") {
    import org.apache.commons.numbers.gamma.LogGamma
    for (a, b) <- Seq((0.3, 4.0), (2.5, 1.2), (40.0, 0.05)); k <- 0 to 60 do
      val nb = NegativeBinomial(a, b)
      val p = 1.0 / (1.0 + b)
      val expected = LogGamma.value(a + k) - LogGamma.value(a) - LogGamma.value(k + 1.0) + a * math.log(p) +
        k * math.log1p(-p)
      assertEqualsDouble(
        nb.logProbabilityOf(k),
        expected,
        1e-12 * math.max(1.0, math.abs(expected)),
        s"a=$a, b=$b, k=$k"
      )
    end for
  }

  test("volume-weighted MLE validates its arguments") {
    intercept[IllegalArgumentException](NegativeBinomial.volweightedMle(Array(1, 2), Array(1.0, 1.0), maxIter = 0))
    intercept[IllegalArgumentException](NegativeBinomial.volweightedMle(Array(1, 2), Array(1.0, 1.0), tol = Double.NaN))
    intercept[IllegalArgumentException](NegativeBinomial.volweightedMle(Array(0, 0), Array(1.0, 1.0)))
  }

  test("MLE returns Poisson for underdispersed data") {
    // Data with variance <= mean should return Poisson
    val poissonData = Array.fill(10000)(
      org.apache.commons.statistics.distribution.PoissonDistribution
        .of(5.0)
        .createSampler(org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create())
        .sample()
    )

    val (fitted, converged) = NegativeBinomial.mle(poissonData)
    assert(converged)

    fitted match
      case pois: Poisson =>
        assertEqualsDouble(pois.lambda, 5.0, 0.3)
      case _: NegativeBinomial =>
        // Also acceptable if slightly overdispersed due to sampling variance
        ()
    end match
  }

  test("JSON codec round-trips a and b") {
    import io.circe.syntax.*
    import io.circe.parser.decode
    val nb = NegativeBinomial(2.5, 0.75)
    assertEquals(nb.asJson.noSpaces, """{"a":2.5,"b":0.75}""")
    assertEquals(decode[NegativeBinomial](nb.asJson.noSpaces), Right(nb))
  }

  test("JSON decoder reports invalid parameters as a decoding failure") {
    import io.circe.parser.decode
    assert(decode[NegativeBinomial]("""{"a":1.0,"b":0.0}""").isLeft)
  }

  test("poisson(mu) limit matches Poisson pmf without LogGamma cancellation") {
    val nb = NegativeBinomial.poisson(5.0)
    val p = Poisson(5.0)
    for k <- 0 to 20 do
      val rel = math.abs(nb.probabilityOf(k) / p.probabilityOf(k) - 1.0)
      assert(rel < 1e-9, s"k=$k rel err=$rel")
    end for
  }

end NegBinTest
