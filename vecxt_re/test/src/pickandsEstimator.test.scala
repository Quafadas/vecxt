package vecxt_re

import PickandsEstimatorExtensions.*

class PickandsEstimatorSuite extends munit.FunSuite:

  // Helper to generate Pareto samples using inverse transform
  def generatePareto(n: Int, alpha: Double, xMin: Double = 1.0, seed: Long = 42L): Array[Double] =
    val rng = new scala.util.Random(seed)
    Array.fill(n) {
      val u = rng.nextDouble()
      xMin / math.pow(u, 1.0 / alpha)
    }

  test("Pickands estimator basic formula check") {
    // Construct a simple case where we know the order statistics
    // Data: 1, 2, 3, 4, 5, 6, 7, 8, 9 (n=9)
    // k=2: X_(n-k)=X_(7)=8, X_(n-2k)=X_(5)=6, X_(n-4k)=X_(1)=2
    // γ = ln((8-6)/(6-2)) / ln(2) = ln(2/4) / ln(2) = ln(0.5) / ln(2) = -1
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    val gamma = data.pickandsEstimator(2)
    assertEqualsDouble(gamma, -1.0, 1e-10)
  }

  test("Pickands estimator for Pareto(2.0) distribution converges") {
    val alpha = 2.0
    val gamma = 1.0 / alpha // = 0.5
    val data = generatePareto(10000, alpha)

    // Pickands has higher variance - use multiple k values and average
    val result = data.pickandsPlot(kMin = 100, kMax = 500, step = 10)
    val validEstimates = result.gammaEstimates.filter(g => g > 0 && !g.isNaN)
    val meanEstimate = validEstimates.sum / validEstimates.length

    // Pickands has higher variance than Hill, so allow 50% error
    assertEqualsDouble(meanEstimate, gamma, gamma * 0.5)
  }

  test("Pickands tail index for Pareto(2.0)") {
    val alpha = 2.0
    val data = generatePareto(10000, alpha)

    // Average over a range of k values for more stable estimate
    val result = data.pickandsPlot(kMin = 100, kMax = 500, step = 10)
    val validEstimates = result.alphaEstimates.filter(a => a > 0 && !a.isNaN && a.isFinite)
    val meanEstimate = validEstimates.sum / validEstimates.length

    // Should be close to 2, allow 50% error for Pickands
    assertEqualsDouble(meanEstimate, alpha, alpha * 0.5)
  }

  test("Pickands estimator for Pareto(1.5) distribution") {
    val alpha = 1.5
    val gamma = 1.0 / alpha
    val data = generatePareto(5000, alpha, seed = 123L)
    val k = 40
    val estimate = data.pickandsEstimator(k)

    assertEqualsDouble(estimate, gamma, gamma * 0.35)
  }

  test("Pickands estimator rejects invalid k values") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0) // n=9

    intercept[IllegalArgumentException] {
      data.pickandsEstimator(0) // k must be >= 1
    }

    intercept[IllegalArgumentException] {
      data.pickandsEstimator(3) // 4*3=12 > 9
    }
  }

  test("Pickands estimator rejects small arrays") {
    intercept[IllegalArgumentException] {
      Array(1.0, 2.0, 3.0, 4.0).pickandsEstimator(1)
    }
  }

  test("Pickands plot produces valid output") {
    val alpha = 2.0
    val data = generatePareto(1000, alpha)
    val result = data.pickandsPlot(kMin = 5, kMax = 50, step = 5)

    assertEquals(result.kValues.length, result.gammaEstimates.length)
    assertEquals(result.kValues.length, result.alphaEstimates.length)
    assert(result.kValues.head == 5)
    assert(result.kValues.last == 50)
  }

  test("Pickands plot default kMax respects 4k < n constraint") {
    val data = generatePareto(100, 2.0)
    val result = data.pickandsPlot()

    // Max valid k = (100-1)/4 = 24
    assert(result.kValues.last <= 24)
  }

  test("Pickands plot positive gamma implies positive alpha") {
    val data = generatePareto(500, 2.0)
    val result = data.pickandsPlot(kMin = 2, kMax = 20)

    result.gammaEstimates.zip(result.alphaEstimates).foreach { case (gamma, alpha) =>
      if gamma > 0 && !gamma.isNaN then
        assert(alpha > 0, s"Expected positive alpha for gamma=$gamma")
        assertEqualsDouble(alpha, 1.0 / gamma, 1e-10)
      end if
    }
  }

  test("Pickands estimator is invariant to data order") {
    val data = Array(9.0, 1.0, 5.0, 3.0, 7.0, 2.0, 8.0, 4.0, 6.0)
    val shuffled = scala.util.Random.shuffle(data.toSeq).toArray

    val est1 = data.pickandsEstimator(2)
    val est2 = shuffled.pickandsEstimator(2)

    assertEqualsDouble(est1, est2, 1e-10)
  }

  test("Pickands findStableRegion identifies plateau") {
    val alpha = 2.0
    val data = generatePareto(2000, alpha, seed = 456L)
    val result = data.pickandsPlot(kMin = 10, kMax = 100, step = 2)

    result.findStableRegion(windowSize = 5, threshold = 0.3) match
      case Some((kStart, kEnd, meanGamma, meanAlpha)) =>
        // Mean alpha should be close to true alpha
        assertEqualsDouble(meanAlpha, alpha, alpha * 0.35)
        assert(kStart < kEnd)
        assertEqualsDouble(meanGamma, 1.0 / meanAlpha, 1e-10)
      case None =>
        // Okay if no stable region found with strict threshold
        ()
  }

  test("Pickands plot step parameter works correctly") {
    val data = generatePareto(500, 2.0)
    val result = data.pickandsPlot(kMin = 5, kMax = 25, step = 5)

    assertEquals(result.kValues.toSeq, Seq(5, 10, 15, 20, 25))
    assertEquals(result.gammaEstimates.length, 5)
  }

end PickandsEstimatorSuite
