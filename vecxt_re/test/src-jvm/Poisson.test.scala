package vecxt_re

import munit.FunSuite
import vecxt.all.*
import org.apache.commons.statistics.distribution.PoissonDistribution as ApachePoisson

class PoissonTest extends FunSuite:

  inline val localTests = true

  test("pmf approximately normalizes") {
    val pois = Poisson(lambda = 5.0)

    val mu = pois.mean
    val sd = math.sqrt(pois.variance)
    val K = (mu + 15 * sd).toInt

    val sum = (0 to K).map(pois.probabilityOf).sum

    assert(math.abs(sum - 1.0) < 1e-8)
  }

  test("pmf mean and variance match theory") {
    val lambda = 7.5
    val pois = Poisson(lambda)

    val K = 500
    val probs = (0 to K).map(k => pois.probabilityOf(k))

    val mean = probs.zipWithIndex.map { case (p, k) => p * k }.sum
    val varr = probs.zipWithIndex.map { case (p, k) => p * k * k }.sum - mean * mean

    // For Poisson, mean = variance = lambda
    assert(math.abs(mean - lambda) < 1e-6)
    assert(math.abs(varr - lambda) < 1e-6)
    assert(math.abs(pois.mean - lambda) < 1e-10)
    assert(math.abs(pois.variance - lambda) < 1e-10)
  }

  test("matches Apache Commons Poisson distribution") {
    val lambda = 4.0
    val pois = Poisson(lambda)
    val apachePois = ApachePoisson.of(lambda)

    assert(pois.probabilityOf(-1) == 0.0)
    assert(pois.logProbabilityOf(-1).isNegInfinity)

    (0 to 20).foreach { k =>
      val diff = math.abs(pois.probabilityOf(k) - apachePois.probability(k))
      assert(diff < 1e-14, s"PMF mismatch at k=$k: ${pois.probabilityOf(k)} vs ${apachePois.probability(k)}")
    }
  }

  test("cdf matches Apache Commons") {
    val lambda = 6.0
    val pois = Poisson(lambda)
    val apachePois = ApachePoisson.of(lambda)

    (0 to 25).foreach { k =>
      val diff = math.abs(pois.cdf(k) - apachePois.cumulativeProbability(k))
      assert(diff < 1e-12, s"CDF mismatch at k=$k: ${pois.cdf(k)} vs ${apachePois.cumulativeProbability(k)}")
    }
  }

  test("probability(x, y) equals cdf(y) - cdf(x)") {
    val pois = Poisson(5.0)

    for
      x <- 0 to 10
      y <- (x + 1) to 15
    do
      val expected = pois.cdf(y) - pois.cdf(x)
      val actual = pois.probability(x, y)
      assert(math.abs(actual - expected) < 1e-14)
  }

  test("small lambda works correctly") {
    val pois = Poisson(0.1)

    val K = 50
    val sum = (0 to K).map(pois.probabilityOf).sum
    assert(math.abs(sum - 1.0) < 1e-10)

    // P(X=0) = e^(-0.1) ≈ 0.9048
    assert(math.abs(pois.probabilityOf(0) - math.exp(-0.1)) < 1e-14)
  }

  test("large lambda works correctly") {
    val pois = Poisson(100.0)

    // For large lambda, distribution is approximately normal with mean=variance=lambda
    val K = 250
    val probs = (0 to K).map(k => pois.probabilityOf(k))
    val sum = probs.sum

    assert(math.abs(sum - 1.0) < 1e-6)

    val mean = probs.zipWithIndex.map { case (p, k) => p * k }.sum
    assert(math.abs(mean - 100.0) < 1e-4)
  }

  // Ignored in CI as slow
  test("SLOW: sampling mean and variance") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.PoissonTest.sampling mean and variance IN CI========")
    val pois = Poisson(8.0)
    val n = 2_000_000

    val xs = Array.fill(n)(pois.draw.toDouble)

    val mean = xs.sum / n
    val varr = xs.map(x => (x - mean) * (x - mean)).sum / n

    assert(math.abs(mean - pois.mean) < 5e-3)
    assert(math.abs(varr - pois.variance) < 5e-2)
  }

  test("SLOW: sampling distribution matches pmf") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF vecxt_re.PoissonTest.sampling distribution matches pmf IN CI========")
    val pois = Poisson(4.0)
    val n = 500_000

    val samples = Array.fill(n)(pois.draw)
    val counts = samples.groupBy(identity).view.mapValues(_.size).toMap

    (0 to 15).foreach { k =>
      val empirical = counts.getOrElse(k, 0).toDouble / n
      val theoretical = pois.probabilityOf(k)
      val diff = math.abs(empirical - theoretical)
      assert(diff < 0.01, s"At k=$k: empirical=$empirical, theoretical=$theoretical, diff=$diff")
    }
  }

  test("SLOW: MLE recovers true parameter") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF MLE recovers true parameter distribution matches pmf IN CI========")

    val trueLambda = 6.0
    val apachePois = ApachePoisson.of(trueLambda)
    val sampler = apachePois.createSampler(org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create())

    val data = Array.fill(10_000)(sampler.sample())
    val (fitted, converged) = Poisson.mle(data)

    assert(converged)
    assert(math.abs(fitted.lambda - trueLambda) < 0.1, s"Fitted lambda=${fitted.lambda}, true=$trueLambda")
  }

  test("SLOW: MLE volume-weighted with uniform volumes equals regular MLE") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF MLE volume-weighted with uniform volumes equals regular MLE IN CI========")


    val trueLambda = 5.0
    val apachePois = ApachePoisson.of(trueLambda)
    val sampler = apachePois.createSampler(org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create())

    val data = Array.fill(10_000)(sampler.sample())
    val uniformVolumes = Array.fill(10_000)(1.0)

    val (fitted, converged) = Poisson.mle(data)
    val (fittedVol, convergedVol) = Poisson.volweightedMle(data, uniformVolumes)

    assert(converged)
    assert(convergedVol)
    assert(math.abs(fitted.lambda - fittedVol.lambda) < 1e-10)
  }

  test("SLOW: volume-weighted MLE correctly adjusts for volumes") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF volume-weighted MLE correctly adjusts for volumes IN CI========")
    // If we have counts n_j from volumes v_j, the rate lambda should be sum(n_j) / sum(v_j)
    val observations = Array(10, 20, 15, 25)
    val volumes = Array(2.0, 4.0, 3.0, 5.0)

    val expectedLambda = observations.sum.toDouble / volumes.sum // = 70 / 14 = 5.0
    val (fitted, converged) = Poisson.volweightedMle(observations, volumes)

    assert(converged)
    assert(math.abs(fitted.lambda - expectedLambda) < 1e-10)
  }

  test("SLOW: goodness-of-fit test accepts Poisson data") {
    assume(localTests, "Don't run local-only tests in CI ideally as they are slow")
    println("=============TURN OFF goodness-of-fit test accepts Poisson data IN CI========")
    val trueLambda = 5.0
    val apachePois = ApachePoisson.of(trueLambda)
    val sampler = apachePois.createSampler(org.apache.commons.rng.simple.RandomSource.XO_RO_SHI_RO_128_PP.create())

    val data = Array.fill(1000)(sampler.sample())
    val (statistic, df, pValue) = Poisson.goodnessOfFit(data)

    // With Poisson data, we should not reject at α=0.05
    assert(pValue > 0.01, s"p-value=$pValue is suspiciously low for Poisson data")
  }

  test("fromMean creates distribution with correct lambda") {
    val mu = 7.5
    val pois = Poisson.fromMean(mu)
    assert(pois.lambda == mu)
    assert(pois.mean == mu)
    assert(pois.variance == mu)
  }

end PoissonTest
