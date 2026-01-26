package vecxt_re

import munit.FunSuite
import vecxt.all.*
import org.apache.commons.statistics.distribution.PoissonDistribution

class NegBinTest extends FunSuite:

  inline val localTests = true

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

    // println(s"True parameters: a=${trueNb.a}, b=${trueNb.b}")
    // println(s"Fitted parameters: a=${fitted.a}, b=${fitted.b}")

    assertEqualsDouble(fitted.mean, trueNb.mean, 0.1)
    assertEqualsDouble(fitted.b, trueNb.b, 0.1)
  }

  test("SLOW: vol weighted MLE follows standard case with uniform volumes ") {

    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.MLE recovers parameters IN CI========")

    val trueNb = NegativeBinomial(4.0, 0.6)
    val data = Array.fill(10_000)(trueNb.draw)

    val (fitted, converged) = NegativeBinomial.mleVolumeWeighted(data, Array.fill(10_000)(1.0))
    assert(converged)

    assertEqualsDouble(fitted.mean, trueNb.mean, 0.1)
    assertEqualsDouble(fitted.b, trueNb.b, 0.1)
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

    val (fitted, converged) = NegativeBinomial.mleVolumeWeighted(data, vols, maxIter = 200, tol = 1e-8)
    assert(converged)
    assertEqualsDouble(fitted.a, rTrue, 0.1)
    assertEqualsDouble(fitted.b, betaTrue, 0.1)

    // Ignoring volumes collapses a mixture of scaled NB's into a single NB, which should fit worse
    // (at minimum: it should be less accurate on the modeled-period mean and dispersion).
    val modeledMean = rTrue * betaTrue
    val (unweighted, _) = NegativeBinomial.mle(data)
    assert(math.abs(fitted.mean - modeledMean) <= math.abs(unweighted.mean - modeledMean))
    assert(math.abs(fitted.b - betaTrue) <= math.abs(unweighted.b - betaTrue))
  }

end NegBinTest
