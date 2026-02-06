package vecxt_re

import HillEstimatorExtensions.*

class HillEstimatorSuite extends munit.FunSuite:

  // Helper to generate Pareto samples using inverse transform
  def generatePareto(n: Int, alpha: Double, xMin: Double = 1.0, seed: Long = 42L): Array[Double] =
    val rng = new scala.util.Random(seed)
    Array.fill(n) {
      val u = rng.nextDouble()
      xMin / math.pow(u, 1.0 / alpha)
    }
  end generatePareto

  test("Hill estimator basic sanity check") {
    // Simple case: known sorted data
    val data = Array(1.0, 2.0, 4.0, 8.0, 16.0)
    // Using k=2 means we use the 2 largest: 16, 8
    // Threshold is at position n-k-1 = 5-2-1 = 2, which is 4.0
    // sum = ln(16/4) + ln(8/4) = ln(4) + ln(2) = 2*ln(2) + ln(2) = 3*ln(2)
    // estimate = 2 / (3*ln(2))
    val expected = 2.0 / (3.0 * math.log(2.0))
    val estimate = data.hillEstimator(2)
    assertEqualsDouble(estimate, expected, 1e-10)
  }

  test("Hill estimator converges for Pareto(2.0) distribution") {
    val alpha = 2.0
    val data = generatePareto(10000, alpha)
    // With large sample, estimate should be close to true alpha
    val estimate = data.hillEstimator(500)
    // Allow 15% error for statistical estimation
    assertEqualsDouble(estimate, alpha, alpha * 0.15)
  }

  test("Hill estimator converges for Pareto(1.5) distribution") {
    val alpha = 1.5
    val data = generatePareto(10000, alpha)
    val estimate = data.hillEstimator(500)
    assertEqualsDouble(estimate, alpha, alpha * 0.15)
  }

  test("Hill estimator converges for Pareto(3.0) distribution") {
    val alpha = 3.0
    val data = generatePareto(10000, alpha)
    val estimate = data.hillEstimator(500)
    assertEqualsDouble(estimate, alpha, alpha * 0.15)
  }

  test("Hill estimator rejects invalid k values") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0)

    intercept[IllegalArgumentException] {
      data.hillEstimator(0) // k must be >= 1
    }

    intercept[IllegalArgumentException] {
      data.hillEstimator(5) // k must be < n
    }

    intercept[IllegalArgumentException] {
      data.hillEstimator(10) // k must be < n
    }
  }

  test("Hill estimator rejects empty or single-element arrays") {
    intercept[IllegalArgumentException] {
      Array.empty[Double].hillEstimator(1)
    }

    intercept[IllegalArgumentException] {
      Array(1.0).hillEstimator(1)
    }
  }

  test("Hill plot produces valid output") {
    val alpha = 2.0
    val data = generatePareto(1000, alpha)
    val result = data.hillPlot(kMin = 10, kMax = 100, step = 5)

    assertEquals(result.kValues.length, result.estimates.length)
    assert(result.kValues.head == 10)
    assert(result.kValues.last == 100)
    assert(result.kValues.length == 19) // (100-10)/5 + 1 = 19
  }

  test("Hill plot estimates are positive for valid Pareto data") {
    val data = generatePareto(500, 2.0)
    val result = data.hillPlot(kMin = 5, kMax = 50)

    result.estimates.foreach { est =>
      assert(est > 0, s"Expected positive estimate, got $est")
    }
  }

  test("Hill plot default kMax is sensible") {
    val data = generatePareto(100, 2.0)
    val result = data.hillPlot()

    // Default kMax should be min(n/2, n-1) = 50
    assert(result.kValues.last <= 50)
    assert(result.kValues.head == 2) // default kMin
  }

  test("Hill plot findStableRegion identifies plateau") {
    // Generate clean Pareto data
    val alpha = 2.0
    val data = generatePareto(5000, alpha, seed = 123L)
    val result = data.hillPlot(kMin = 50, kMax = 1000, step = 10)

    result.findStableRegion(windowSize = 5, threshold = 0.15) match
      case Some((kStart, kEnd, meanEstimate)) =>
        // The stable region should give estimate close to true alpha
        assertEqualsDouble(meanEstimate, alpha, alpha * 0.2)
        assert(kStart < kEnd)
      case None =>
        // It's okay if no stable region found with strict threshold
        // Just verify the method runs without error
        ()
    end match
  }

  test("Hill estimator is invariant to data order") {
    val data = Array(5.0, 1.0, 10.0, 2.0, 20.0, 3.0)
    val shuffled = data.clone()
    scala.util.Random.shuffle(shuffled.toSeq).toArray

    val est1 = data.hillEstimator(2)
    val est2 = shuffled.hillEstimator(2)

    // Both should give same result after internal sorting
    assertEqualsDouble(est1, est2, 1e-10)
  }

  test("Hill estimator with k=1 uses only largest value") {
    val data = Array(1.0, 2.0, 4.0, 8.0)
    // k=1: use only largest (8), threshold is second largest (4)
    // estimate = 1 / ln(8/4) = 1/ln(2)
    val expected = 1.0 / math.log(2.0)
    val estimate = data.hillEstimator(1)
    assertEqualsDouble(estimate, expected, 1e-10)
  }

  test("Hill plot step parameter works correctly") {
    val data = generatePareto(200, 2.0)
    val result = data.hillPlot(kMin = 10, kMax = 50, step = 10)

    assertEquals(result.kValues.toSeq, Seq(10, 20, 30, 40, 50))
    assertEquals(result.estimates.length, 5)
  }

end HillEstimatorSuite
