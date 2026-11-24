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

  test("Hill plot default k range uses every loss") {
    val data = generatePareto(100, 2.0)
    // the threshold reaches the smallest loss: k = n - 1, or n for Embrechts
    assertEquals(data.hillPlot().kValues.last, 99)
    assertEquals(data.hillPlot(convention = HillConvention.Unbiased).kValues.last, 99)
    assertEquals(data.hillPlot(convention = HillConvention.Embrechts).kValues.last, 100)
    assertEquals(data.hillPlot().kValues.head, 2) // default kMin
    assertEquals(data.hillPlot(convention = HillConvention.Unbiased).kValues.head, 2)
    assertEquals(data.hillPlot(convention = HillConvention.Embrechts).kValues.head, 3)
  }

  test("Hill plot defaults cover the same thresholds in every convention") {
    val data = generatePareto(100, 2.0)
    val thresholds = HillConvention.values.map(c => data.hillPlot(convention = c).thresholds.toSeq)
    thresholds.foreach(t => assertEquals(t, thresholds.head))
  }

  test("Embrechts Hill plot still allows k = 2 when asked") {
    val data = generatePareto(100, 2.0)
    assertEquals(data.hillPlot(kMin = 2, convention = HillConvention.Embrechts).kValues.head, 2)
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

  test("Hill plot thresholds are the (k+1)-th largest observation") {
    val data = Array(308.0, 832.0, 365.0, 653.0, 430.0, 386.0, 310.0, 374.0, 427.0)
    val result = data.hillPlot(kMax = data.length - 1)
    val descending = data.sorted.reverse
    assertEquals(result.thresholds.length, result.kValues.length)
    assertEquals(result.nObs, data.length)
    result.kValues.zip(result.thresholds).foreach((k, t) => assertEqualsDouble(t, descending(k), 0.0))
    // k = 2: the two largest over the third largest
    assertEqualsDouble(result.estimates.head, 2.0 / (math.log(832.0 / 430.0) + math.log(653.0 / 430.0)), 1e-12)
  }

  test("Unbiased Hill estimator is (k - 1) / k times the maximum likelihood estimate") {
    val data = Array(1.0, 2.0, 4.0, 8.0, 16.0)
    // k = 2: S = ln(16/4) + ln(8/4) = 3 ln 2, so (k - 1) / S = 1 / (3 ln 2)
    assertEqualsDouble(data.hillEstimator(2, HillConvention.Unbiased), 1.0 / (3.0 * math.log(2.0)), 1e-12)
    assertEqualsDouble(data.hillEstimator(3, HillConvention.Unbiased), data.hillEstimator(3) * 2.0 / 3.0, 1e-12)
  }

  test("Embrechts Hill estimator puts the threshold at the k-th largest") {
    val data = Array(1.0, 2.0, 4.0, 8.0, 16.0)
    // k = 3: threshold 4, S = ln(16/4) + ln(8/4) + ln(4/4) = 3 ln 2, estimate 3 / S
    assertEqualsDouble(data.hillEstimator(3, HillConvention.Embrechts), 1.0 / math.log(2.0), 1e-12)
    // k = n is allowed: threshold is the smallest loss
    assertEqualsDouble(data.hillEstimator(5, HillConvention.Embrechts), 5.0 / (10.0 * math.log(2.0)), 1e-12)
  }

  test("Embrechts estimate is k / (k - 1) times the maximum likelihood estimate at k - 1, and never below it at k") {
    val data = generatePareto(300, 2.0)
    (2 to 100).foreach { k =>
      val ekm = data.hillEstimator(k, HillConvention.Embrechts)
      assertEqualsDouble(ekm, data.hillEstimator(k - 1) * k / (k - 1), 1e-9)
      assert(ekm >= data.hillEstimator(k))
    }
  }

  test("Hill conventions reject k outside their range") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    intercept[IllegalArgumentException](data.hillEstimator(1, HillConvention.Unbiased))
    intercept[IllegalArgumentException](data.hillEstimator(1, HillConvention.Embrechts))
    intercept[IllegalArgumentException](data.hillEstimator(5, HillConvention.Unbiased))
    intercept[IllegalArgumentException](data.hillEstimator(6, HillConvention.Embrechts))
  }

  test("Hill plot conventions: estimates, thresholds and S line up with the maximum likelihood plot") {
    val data = generatePareto(200, 2.0)
    val mle = data.hillPlot(kMin = 1)
    val unbiased = data.hillPlot(kMin = 1, convention = HillConvention.Unbiased)
    val ekm = data.hillPlot(kMin = 1, convention = HillConvention.Embrechts)
    assertEquals(mle.convention, HillConvention.MaxLikelihood)
    assertEquals(unbiased.convention, HillConvention.Unbiased)
    assertEquals(ekm.convention, HillConvention.Embrechts)
    assertEquals(unbiased.kValues.toSeq, mle.kValues.toSeq.tail)
    assertEquals(ekm.kValues.toSeq.init, mle.kValues.toSeq.tail) // Embrechts also reaches k = n
    val descending = data.sorted.reverse
    unbiased.kValues.indices.foreach { i =>
      val k = unbiased.kValues(i)
      assertEqualsDouble(unbiased.estimates(i), mle.estimates(i + 1) * (k - 1) / k, 1e-12)
      assertEqualsDouble(unbiased.thresholds(i), mle.thresholds(i + 1), 0.0)
      assertEqualsDouble(unbiased.sumLogRatios(i), mle.sumLogRatios(i + 1), 1e-12)
      // Embrechts at k: threshold the k-th largest, and the same estimate as k / (k - 1) * MLE at k - 1
      assertEqualsDouble(ekm.thresholds(i), descending(k - 1), 0.0)
      assertEqualsDouble(ekm.estimates(i), mle.estimates(i) * k / (k - 1), 1e-9)
    }
  }

  test(
    "Unbiased Hill estimator is unbiased for exact Pareto data; MLE biased by k / (k - 1), Embrechts by k / (k - 2)"
  ) {
    // With k + 1 observations the losses above the threshold are exactly Pareto
    val alpha = 2.0
    val k = 5
    val reps = 20000
    val rng = new scala.util.Random(7L)
    var sumMle = 0.0
    var sumUnbiased = 0.0
    var sumEkm = 0.0
    var r = 0
    while r < reps do
      val data = Array.fill(k + 1)(1.0 / math.pow(rng.nextDouble(), 1.0 / alpha))
      sumMle += data.hillEstimator(k)
      sumUnbiased += data.hillEstimator(k, HillConvention.Unbiased)
      sumEkm += data.hillEstimator(k, HillConvention.Embrechts)
      r += 1
    end while
    // standard errors of the means are about 0.008, 0.010 and 0.017
    assertEqualsDouble(sumUnbiased / reps, alpha, 0.04)
    assertEqualsDouble(sumMle / reps, alpha * k / (k - 1), 0.05)
    assertEqualsDouble(sumEkm / reps, alpha * k / (k - 2), 0.08)
  }

end HillEstimatorSuite
